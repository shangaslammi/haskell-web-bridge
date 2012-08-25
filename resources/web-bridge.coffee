# WebBridge Client-side Bootstrap

socket = new WebSocket("ws://localhost:8000/web-bridge")
socket.onmessage = (msg) ->
    console.log msg
    data = JSON.parse msg.data
    switch data.method
        when "eval"
            reqId = data.reqId
            response =
                result: eval data.code
                reqId: reqId
            socket.send JSON.stringify response
        when "async"
            eval data.code

