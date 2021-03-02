Stream = require('node-rtsp-stream');


var url = process.argv[2]
var port = process.argv[3]

stream = new Stream({
  name: 'cam',
  streamUrl: url,
  wsPort: port,
  ffmpegOptions: { // options ffmpeg flags
    '-stats': '', // an option with no neccessary value uses a blank string
    '-r': 30 // options with required values specify the value after the key
  }
})

