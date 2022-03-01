if [ $# -eq 0 ]; then
    echo "usage: openwrt_wpn_setup.sh <wan-ip>"
    exit 1
fi

WAN_ADDR = $1

# Install packages
opkg update
opkg install openvpn-openssl openvpn-easy-rsa
opkg install luci-app-openvpn ipset

# Configuration parameters  # OVPN_POOL config any network are OK except your local network
export OVPN_DIR="/etc/openvpn"
export OVPN_PKI="/etc/easy-rsa/pki"
export OVPN_PORT="1194"
export OVPN_PROTO="udp"
export OVPN_POOL="192.168.8.0 255.255.255.0"
export OVPN_DNS="${OVPN_POOL%.* *}.1"
export OVPN_DOMAIN="$(uci get dhcp.@dnsmasq[0].domain)"

# Fetch WAN IP address
export OVPN_SERV="${WAN_ADDR}"

# Configuration parameters
export EASYRSA_PKI="${OVPN_PKI}"
export EASYRSA_REQ_CN="ovpnca"
export EASYRSA_BATCH="1"

# Remove and re-initialize PKI directory
easyrsa init-pki

# Generate DH parameters
easyrsa gen-dh

# Create a new CA
easyrsa build-ca nopass

# Generate server keys and certificate
easyrsa build-server-full server nopass
openvpn --genkey tls-crypt-v2-server ${EASYRSA_PKI}/private/server.pem

# Generate client keys and certificate
easyrsa build-client-full client nopass
openvpn --tls-crypt-v2 ${EASYRSA_PKI}/private/server.pem \
        --genkey tls-crypt-v2-client ${EASYRSA_PKI}/private/client.pem

# Configure firewall
uci rename firewall.@zone[0]="lan"
uci rename firewall.@zone[1]="wan"
uci del_list firewall.lan.device="tun+"
uci add_list firewall.lan.device="tun+"
uci -q delete firewall.ovpn
uci set firewall.ovpn="rule"
uci set firewall.ovpn.name="Allow-OpenVPN"
uci set firewall.ovpn.src="wan"
uci set firewall.ovpn.dest_port="${OVPN_PORT}"
uci set firewall.ovpn.proto="${OVPN_PROTO}"
uci set firewall.ovpn.target="ACCEPT"
uci commit firewall
/etc/init.d/firewall restart

# Show Firewall config
uci show firewall.ovpn

# Create OpenVPN server
uci set openvpn.server="openvpn"
uci set openvpn.server.enabled="1"
uci set openvpn.server.config="${OVPN_DIR}/server.conf"
uci set openvpn.server.dev="tun"
uci set openvpn.server.port="1193"
uci set openvpn.server.proto="udp"
uci set openvpn.server.comp_lzo="no"
uci set openvpn.server.log="/tmp/openvpn.log"
uci set openvpn.server.status="/var/log/openvpn.log"
uci set openvpn.server.verb="3"
uci set openvpn.server.mute="5"
uci set openvpn.server.keepalive="10 120"
uci set openvpn.server.persist_key="1"
uci set openvpn.server.persist_tun="1"
uci set openvpn.server.user="nobody"
uci set openvpn.server.group="nogroup"
uci set openvpn.server.ca="${EASYRSA_PKI}/ca.crt"
uci set openvpn.server.cert="${EASYRSA_PKI}/issued/server.crt"
uci set openvpn.server.dh="${EASYRSA_PKI}/dh.pem"
uci set openvpn.server.key="${EASYRSA_PKI}/private/server.key"
uci set openvpn.server.mode="server"
uci set openvpn.server.server="192.168.0.0 255.255.255.0"
uci set openvpn.server.tls_server="1"
uci set openvpn.server.topology="subnet"
uci set openvpn.server.route_gateway="dhcp"
uci set openvpn.server.client_to_client="1"
uci commit openvpn

# Set Clients options
uci add_list openvpn.server.push="comp-lzo no"
uci add_list openvpn.server.push="persist-key"
uci add_list openvpn.server.push="persist-tun"
uci add_list openvpn.server.push="user nobody"
uci add_list openvpn.server.push="user nogroup"
uci add_list openvpn.server.push="topology subnet"
uci add_list openvpn.server.push="route-gateway dhcp"
uci add_list openvpn.server.push="redirect-gateway def1"
uci add_list openvpn.server.push="192.168.0.0 255.255.255.0"
uci add_list openvpn.server.push="dhcp-option DNS ${OVPN_DNS}"
uci add_list openvpn.server.push="dhcp-option DNS 1.1.1.1"
uci commit openvpn

# Ensure OpenVPN is enable and restart it
/etc/init.d/openvpn enable
/etc/init.d/openvpn restart

# Configure VPN service and generate client profiles
umask go=
OVPN_DH="$(cat ${OVPN_PKI}/dh.pem)"
OVPN_CA="$(openssl x509 -in ${OVPN_PKI}/ca.crt)"
ls ${OVPN_PKI}/issued \
    | sed -e "s/\.\w*$//" \
    | while read -r OVPN_ID
do
    OVPN_TC="$(cat ${OVPN_PKI}/private/${OVPN_ID}.pem)"
    OVPN_KEY="$(cat ${OVPN_PKI}/private/${OVPN_ID}.key)"
    OVPN_CERT="$(openssl x509 -in ${OVPN_PKI}/issued/${OVPN_ID}.crt)"
    OVPN_EKU="$(echo "${OVPN_CERT}" | openssl x509 -noout -purpose)"
    case ${OVPN_EKU} in
        (*"SSL server : Yes"*)
            OVPN_CONF="${OVPN_DIR}/${OVPN_ID}.conf"
            cat << EOF > ${OVPN_CONF} ;;
user nobody
group nogroup
dev tun
port ${OVPN_PORT}
proto ${OVPN_PROTO}
server ${OVPN_POOL}
topology subnet
client-to-client
keepalive 10 60
persist-tun
persist-key
push "dhcp-option DNS ${OVPN_DNS}"
push "dhcp-option DOMAIN ${OVPN_DOMAIN}"
push "redirect-gateway def1"
push "persist-tun"
push "persist-key"
<dh>
${OVPN_DH}
</dh>
EOF
                                        (*"SSL client : Yes"*)
                                        OVPN_CONF="${OVPN_DIR}/${OVPN_ID}.ovpn"
                                        cat << EOF > ${OVPN_CONF} ;;
user nobody
group nogroup
dev tun
nobind
client
remote ${OVPN_SERV} ${OVPN_PORT} ${OVPN_PROTO}
auth-nocache
remote-cert-tls server
EOF
    esac
    cat << EOF >> ${OVPN_CONF}
<tls-crypt-v2>
${OVPN_TC}
</tls-crypt-v2>
<key>
${OVPN_KEY}
</key>
<cert>
${OVPN_CERT}
</cert>
<ca>
${OVPN_CA}
</ca>
EOF
done
/etc/init.d/openvpn restart
ls ${OVPN_DIR}/*.ovpn
