dune exec bin/main.exe <<EOF
{"version":"2.0", "method": "hello"}
{"version":"2.0", "id": 2, "method": "something"}
{"version":"2.0", "method": "ping"}
{"version":"2.0", "id": 3, "method": "getStatus", "params": ["user123"]}
{"version":"2.0", "id": 4, "method": "disconnect"}
EOF
