import { Elm } from './Main.elm'

const dev = window.location.pathname.includes('localhost')

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: { dev }
})

function listen () {
  let timeout
  const ws = new window.WebSocket(`ws://${window.location.host}`)
  ws.onmessage = function (ev) {
    if (!ev.data) return
    if (timeout) window.clearTimeout(timeout)
    timeout = window.setTimeout(function () {
      ws.close()
      setTimeout(listen, 0)
    }, 8 * 1000)
    app.ports.receivePredictions.send(ev.data)
  }

  ws.onerror = function () { ws.close() }

  ws.onclose = function () { setTimeout(listen, 5000) }
}

listen()
