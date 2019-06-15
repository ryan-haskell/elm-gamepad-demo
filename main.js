import { Elm } from './src/Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('app')
})

const messages = {
  incoming: {
    gamepadConnected: (event) =>
      app.ports.incoming.send({
        type: 'gamepadConnected',
        event
      }),
    gamepadDisconnected: (event) =>
      app.ports.incoming.send({
        type: 'gamepadDisconnected',
        event
      }),
    gamepadUpdated: (event) =>
      app.ports.incoming.send({
        type: 'gamepadConnected',
        event
      })
  }
}

const handlers = {
  outgoing: {
    pollGamepad: (id) => {
      const [ gamepad ] = navigator.getGamepads()
      messages.incoming.gamepadConnected({ gamepad })
    }
  }
}

app.ports.outgoing.subscribe(({ action, data }) =>
  handlers.outgoing[action](data)
)

window.addEventListener('gamepadconnected', messages.incoming.gamepadConnected)
window.addEventListener('gamepaddisconnected', messages.incoming.gamepadDisconnected)
