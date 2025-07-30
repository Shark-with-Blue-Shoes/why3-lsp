dune exec bin/main.exe <<EOF
Content-Length: 6\r\n Content-Type: "json-RPC"\r\n\r\n {"jsonrpc":"2.0", "id": 2, "method": "initialize", "params": {"process_id": 3, "rootUri": "hello"}}
Content-Length: 1000\r\n Content-Type: "json-RPC"\r\n\r\n [{"jsonrpc":"2.0", "id": 3, "method": "initialize", "params": {"process_id": 3, "rootUri": "hello"}},{"jsonrpc":"2.0", "id": 4, "method": "disconnect"}]
EOF
