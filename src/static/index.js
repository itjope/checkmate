var cuid = require('cuid');
require( './styles/main.scss' );

// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
Elm.Main.embed( document.getElementById( 'main' ), {
  cuid: cuid()
});

setTimeout(function() {
  var input = document.getElementById('cm-command-input')
  input.addEventListener('keydown', function(e) {
    if (e.keyCode === 9) {
      e.preventDefault()
    }
  })
}, 0)
