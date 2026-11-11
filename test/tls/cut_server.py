# A TLS server that closes connections without the TLS close message
# (close_notify), as a truncating attacker would.
#   python3 cut_server.py CERT KEY COUNT
# Prints its port, then answers COUNT connections: /framed gets a response
# with Content-Length, anything else one whose body runs until the close.
import socket
import ssl
import sys

ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
ctx.load_cert_chain(sys.argv[1], sys.argv[2])
srv = socket.socket()
srv.bind(("127.0.0.1", 0))
srv.listen(16)
srv.settimeout(120)
print(srv.getsockname()[1], flush=True)
for _ in range(int(sys.argv[3])):
    conn, _ = srv.accept()
    tls = ctx.wrap_socket(conn, server_side=True)
    data = b""
    while b"\r\n\r\n" not in data:
        got = tls.recv(4096)
        if not got:
            break
        data += got
    parts = data.split(b" ")
    path = parts[1] if len(parts) > 1 else b"/"
    if path == b"/framed":
        tls.sendall(b"HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello")
    else:
        tls.sendall(b"HTTP/1.1 200 OK\r\n\r\nuntil close")
    # close the socket under the TLS session: no close_notify
    conn2 = socket.socket(fileno=tls.detach())
    conn2.close()
