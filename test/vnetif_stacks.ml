(* Placeholder stacks used for testing *)

module type V6 = sig
  include Mirage_device.S

  module UDPV6: Mirage_protocols.UDPV6

  module TCPV6: Mirage_protocols.TCPV6

  module IPV6: Mirage_protocols.IPV6

  val udpv6: t -> UDPV6.t
  (** [udpv6 t] obtains a descriptor for use with the [UDPV6] module,
      usually to transmit traffic. *)

  val tcpv6: t -> TCPV6.t
  (** [tcpv6 t] obtains a descriptor for use with the [TCPV6] module,
      usually to initiate outgoing connections. *)

  val ipv6: t -> IPV6.t
  (** [ipv6 t] obtains a descriptor for use with the [IPV6] module,
      which can handle raw IPv6 frames, or manipulate IP address
      configuration on the stack interface. *)

  val listen_udpv6: t -> port:int -> UDPV6.callback -> unit
  (** [listen_udpv6 t ~port cb] registers the [cb] callback on the
      UDPv6 [port] and immediately return.  If [port] is invalid (not
      between 0 and 65535 inclusive), it raises [Invalid_argument].
      Multiple bindings to the same port will overwrite previous
      bindings, so callbacks will not chain if ports clash. *)

  val listen_tcpv6: ?keepalive:Mirage_protocols.Keepalive.t
    -> t -> port:int -> (TCPV6.flow -> unit Lwt.t) -> unit
  (** [listen_tcpv6 ~keepalive t ~port cb] registers the [cb] callback
      on the TCPv6 [port] and immediatey return.  If [port] is invalid (not
      between 0 and 65535 inclusive), it raises [Invalid_argument].
      Multiple bindings to the same port will overwrite previous
      bindings, so callbacks will not chain if ports clash.
      If [~keepalive] is provided then these keepalive settings will be
      applied to the accepted connections before the callback is called. *)

  val listen: t -> unit Lwt.t
  (** [listen t] requests that the stack listen for traffic on the
      network interface associated with the stack, and demultiplex
      traffic to the appropriate callbacks. *)
end

(* Modified from tcpip-stack-direct *)

open Lwt.Infix

let src = Logs.Src.create "tcpip-stack-direct-v6" ~doc:"Pure OCaml TCP/IP stack (IPv6)"
module Log = (val Logs.src_log src : Logs.LOG)

type direct_ipv6_input = src:Ipaddr.V6.t -> dst:Ipaddr.V6.t -> Cstruct.t -> unit Lwt.t
module type UDPV6_DIRECT = Mirage_protocols.UDP
  with type ipaddr = Ipaddr.V6.t
   and type ipinput = direct_ipv6_input

module type TCPV6_DIRECT = Mirage_protocols.TCP
  with type ipaddr = Ipaddr.V6.t
   and type ipinput = direct_ipv6_input

module MakeV6
    (Time     : Mirage_time.S)
    (Random   : Mirage_random.S)
    (Netif    : Mirage_net.S)
    (Ethernet : Mirage_protocols.ETHERNET)
    (Ipv6     : Mirage_protocols.IP with type ipaddr = Ipaddr.V6.t)
    (Udpv6    : UDPV6_DIRECT)
    (Tcpv6    : TCPV6_DIRECT) = struct

  module UDPV6 = Udpv6
  module TCPV6 = Tcpv6
  module IPV6  = Ipv6

  type t = {
    netif : Netif.t;
    ethif : Ethernet.t;
    ipv6  : Ipv6.t;
    udpv6 : Udpv6.t;
    tcpv6 : Tcpv6.t;
    udpv6_listeners: (int, Udpv6.callback) Hashtbl.t;
    tcpv6_listeners: (int, Tcpv6.listener) Hashtbl.t;
  }

  let pp fmt t =
    Format.fprintf fmt "mac=%a,ip=%a" Macaddr.pp (Ethernet.mac t.ethif)
      (Fmt.list Ipaddr.V6.pp) (Ipv6.get_ip t.ipv6)

  let err_invalid_port p = Printf.sprintf "invalid port number (%d)" p

  let tcpv6 { tcpv6; _ } = tcpv6
  let udpv6 { udpv6; _ } = udpv6
  let ipv6 { ipv6; _ } = ipv6

  let listen_udpv6 t ~port callback =
    if port < 0 || port > 65535
    then raise (Invalid_argument (err_invalid_port port))
    else Hashtbl.replace t.udpv6_listeners port callback

  let listen_tcpv6 ?keepalive t ~port process =
    if port < 0 || port > 65535
    then raise (Invalid_argument (err_invalid_port port))
    else Hashtbl.replace t.tcpv6_listeners port { Tcpv6.process; keepalive }

  let udpv6_listeners t ~dst_port =
    try Some (Hashtbl.find t.udpv6_listeners dst_port)
    with Not_found -> None

  let tcpv6_listeners t dst_port =
    try Some (Hashtbl.find t.tcpv6_listeners dst_port)
    with Not_found -> None

  let listen t =
    Log.debug (fun f -> f "Establishing or updating listener for stack %a" pp t);
    let ethif_listener = Ethernet.input
        ~ipv6:(
          Ipv6.input
            ~tcp:(Tcpv6.input t.tcpv6
                    ~listeners:(tcpv6_listeners t))
            ~udp:(Udpv6.input t.udpv6
                    ~listeners:(udpv6_listeners t))
            ~default:(fun ~proto:_ ~src:_ ~dst:_ _ -> Lwt.return_unit)
            t.ipv6)
        ~ipv4:(fun _ -> Lwt.return_unit)
        ~arpv4:(fun _ -> Lwt.return_unit)
        t.ethif
    in
    Netif.listen t.netif ~header_size:Ethernet_wire.sizeof_ethernet ethif_listener
    >>= function
    | Error e ->
      Log.warn (fun p -> p "%a" Netif.pp_error e) ;
      (* XXX: error should be passed to the caller *)
      Lwt.return_unit
    | Ok _res ->
      let nstat = Netif.get_stats_counters t.netif in
      let open Mirage_net in
      Log.info (fun f ->
          f "listening loop of interface %s terminated regularly:@ %Lu bytes \
             (%lu packets) received, %Lu bytes (%lu packets) sent@ "
            (Macaddr.to_string (Netif.mac t.netif))
            nstat.rx_bytes nstat.rx_pkts
            nstat.tx_bytes nstat.tx_pkts) ;
      Lwt.return_unit

  let connect netif ethif ipv6 udpv6 tcpv6 =
    let udpv6_listeners = Hashtbl.create 7 in
    let tcpv6_listeners = Hashtbl.create 7 in
    let t = { netif; ethif; ipv6; tcpv6; udpv6;
              udpv6_listeners; tcpv6_listeners } in
    Log.info (fun f -> f "stack assembled: %a" pp t);
    Lwt.async (fun () -> listen t);
    Lwt.return t

  let disconnect t =
    (* TODO: kill the listening thread *)
    Log.info (fun f -> f "disconnect called (currently a noop): %a" pp t);
    Lwt.return_unit
end
