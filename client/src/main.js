import { Elm } from "./Main.elm"

const dev = window.location.pathname.includes('localhost')

function listen () {
  const ws = new WebSocket(`ws://${window.location.host}`)
  ws.onmessage = function (ev) {
    console.log(ev.data)
    if (!ev.data) return
    console.log(JSON.parse(ev.data))
  }

  ws.onerror = function () {
    ws.close()
  }

  ws.onclose = function () {
    setTimeout(listen, 5000)
  }
}

listen()

Elm.Main.init({
  node: document.getElementById('app'),
  flags: {}
})
