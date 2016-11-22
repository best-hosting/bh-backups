Example 1. Test restored VM by configuring its subnet locally.
--------------------------------------------------------------

Assume two hosts A and B connected directly by cable through interface `eth0`
on both hosts. On host A vpn client has set up for connecting to server to be
restored on virtual machine on host B.  I want to connect vpn from host A to
restored server instead. The server has IP in `46.101.192.0/18` subnet.

### On host B:

Restore the server using procedure above without changing its IP
address.

1. *Define macvtap network* in libvirt:

        <network>
          <name>net-macvtap</name>
          <forward dev='eth0' mode='bridge'>
            <interface dev='eth0'/>
          </forward>
        </network>

    with

        # virsh net-define net-macvtap.xml

    bring up lower macvtap interface (`eth0`):

        # ip link set up dev eth0

    and bring up libvirt network 

        # virsh net-start net-macvtap

    Then define interface inside libvirt domain to use macvtap network:

        <interface type='direct'>
          <source dev='eth0' mode='bridge'/>
          <model type='virtio'/>
        </interface>

    and start VM:

        # virsh start srv

2. *Set up connection between host system and VM*

    VM has following routes and address:

        # ip route list
        default via 46.101.192.1 dev eth0
        46.101.192.0/18 dev eth0  proto kernel  scope link  src 46.101.210.X

        # ip addr show dev eth0
        2: eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
            link/ether 52:54:00:5f:66:68 brd ff:ff:ff:ff:ff:ff
            inet 46.101.210.X/18 brd 46.101.255.255 scope global eth0
               valid_lft forever preferred_lft forever

    Add macvlan interface on host with the same lower interface (`eth0`) as VM
    uses:

        # ip link add link eth0 macvlan0 type macvlan mode bridge

    assign `46.101.192.1` IP to it:

        # ip addr show dev macvlan0
        5: macvlan0@eth0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UNKNOWN group default
            link/ether fe:9c:19:e8:bf:37 brd ff:ff:ff:ff:ff:ff
            inet 46.101.192.1/18 scope global macvlan0
               valid_lft forever preferred_lft forever

    disable arp announces from all interfaces, except the one, where IP belongs
    to:

        # sysctl net.ipv4.conf.all.arp_ignore=1
        # sysctl net.ipv4.conf.all.arp_announce=2

    I need this, because by default on arp queries about IP assigned to
    macvlan interface, host B will answer from both `eth0` (macvlan lower
    interface) and `macvlan0`. With settings above, host B will answer only
    from `macvlan0`.

    I should be able now to ping `46.101.192.1` from VM and ping VM's IP from
    host B. Particularly, that means, that if i try to log in through ssh to
    server from host B, i will connect to VM, not to real server.

    **BE AWARE**: When macvlan interface on host B has been created,
    connection to server will lead to VM instead of real server. So e.g.
    re-running backup will backup VM instead of real server!

3. To allow VM to access internet i need to NAT `46.101.192.0/18` on host B
   and allow forwarding of `46.101.192.0/18` in two directions:

        # iptables -t nat -A POSTROUTING -s 46.101.192.0/18 -j MASQUERADE

        # iptables -A FORWARD -s 46.101.192.0/18 -j ACCEPT
        # iptables -A FORWARD -d 46.101.192.0/18 -j ACCEPT

    Now i should be able to access internet from VM.

### On host A:

To connect to VM from host A i need to add some IP from `46.101.192.0/18`
there too (on `eth0`):

    # ip addr add 46.101.192.2/18 dev eth0

Now log in through ssh from host A should lead to VM, not to real server.

And now all services provided by server should work for host A from restored
server. Particularly, vpn.


Example 2. Test restored VM by NAT-ing to its IP.
-------------------------------------------------

Assume two hosts A and B connected directly by cable through interface `eth0`
on both hosts and use `192.168.4.0/24` subnet there. I want to connect from
host A to site, working on server restored on VM on host B. The server uses
`46.101.192.0/18` subnet.

### On host B:

Restore the server using procedure above without changing its IP
address.

1. Define local nat-based network for VM:

        <network>
          <name>net</name>
          <forward mode='nat'>
            <nat>
              <port start='1024' end='65535'/>
            </nat>
          </forward> <bridge name='virbr1' stp='on' delay='0'/>
          <ip address='46.101.192.1' netmask='255.255.192.0'>
            <dhcp>
              <range start='46.101.192.2' end='46.101.210.62'/>
            </dhcp>
          </ip>
        </network>

    with

        # virsh net-define net.xml

    and bring it up:

        # virsh net-start net

    **BE AWARE**: When libvirt network has brought up, connection to server
    will lead to VM instead of real server. So e.g. re-running backup will
    backup VM instead of real server!

    Define interface inside libvirt domain to use NAT-based network:

        <interface type='network'>
          <source network='net'/>
          <model type='rtl8139'/>
        </interface>

    and then start VM:

        # virsh start srv

2. Add SNAT/DNAT rules for forwarding connections to port 80 on host B to VM:

        # iptables -t nat -R PREROUTING 1 -s 192.168.4.0/29 -d 192.168.4.2 -p tcp --dport 80 -j DNAT --to-destination 46.101.210.X:80
        # iptables -t nat -R POSTROUTING 3 -s 46.101.210.X -p tcp --sport 80 -d 192.168.4.0/29 -j SNAT --to-source  192.168.4.2:80

    where `192.168.4.2` is host B IP address in network, where host A is.  And
    insert rule in `FORWARD` chain allowing forwarding of packets from host A:

        # iptables -I FORWARD 5 -s 192.168.4.0/29 -j ACCEPT

    the complement rule (for forwarding replies) is not necessary, if network
    was setup by libvirt, because by default it will add a rule allowing any
    packets from VM network to be forwarded to the outside.

### On host A

Now if i connect to `192.168.4.2:80`, i should go to site working on restored
server.

