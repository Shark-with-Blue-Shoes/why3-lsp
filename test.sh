dune exec bin/main.exe <<EOF
Content-Length: 3\r\n Content-Type: "json-RPC"\r\n\r\n {"version":"2.0", "method": "hello"}
Content-Length: 6\r\n Content-Type: "json-RPC"\r\n\r\n {"version":"2.0", "id": 2, "method": "initialize", "params": {"process_id": 3}}
Content-Length: 9\r\n Content-Type: "json-RPC"\r\n\r\n {"version":"2.0", "method": "ping"}
Content-Length: 1000\r\n Content-Type: "json-RPC"\r\n\r\n {"version":"2.0", "id": 3, "method": "initialize", "params": ["user123"]}
Content-Length: 6\r\n Content-Type: "json-RPC"\r\n\r\n {"version":"2.0", "id": 4, "method": "disconnect"}
EOF
