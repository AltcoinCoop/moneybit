const {app, BrowserWindow} = require('electron')
const spawn = require('child_process').spawn


let win

const port = '18079'
const cwd = process.cwd()


const server = spawn(cwd + '/bin/moneybit', ['--port', port, '--static', cwd + '/static'])
server.on('close', () => {
    if (win) win.close()
})


function createWindow() {
    win = new BrowserWindow({
        width: 800,
        height: 600,
        "node-integration": false
    })

    win.loadURL('http://localhost:' + port + '/')

    // win.webContents.openDevTools()

    win.on("closed", () => {
        server.kill()
        win = null
    })
}

app.on("ready", createWindow)

app.on("window-all-closed", () => {
    if (process.platform !== "darwin") {
        app.quit()
    }
})

app.on("activate", () => {
    if (win === null) {
        createWindow()
    }
})
