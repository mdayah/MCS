#!/usr/bin/env python3
"""
Local server for DOS emulator.
Run: python3 web/server.py
Open: http://localhost:8080/web/
"""
import http.server, socketserver, os

os.chdir(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

class Handler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Cross-Origin-Embedder-Policy', 'credentialless')
        self.send_header('Cross-Origin-Resource-Policy', 'cross-origin')
        super().end_headers()

print("Server: http://localhost:8080/web/")
socketserver.TCPServer.allow_reuse_address = True
socketserver.TCPServer(("", 8080), Handler).serve_forever()
