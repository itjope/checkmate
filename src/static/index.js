var PouchDB = require('pouchdb')
PouchDB.plugin(require('pouchdb-upsert'))
window.PouchDB = PouchDB
var todosDB = new PouchDB('todos')

var cuid = require('cuid')
require( './styles/main.scss' )

var Elm = require( '../elm/Main' )

var app = Elm.Main.embed( document.getElementById( 'main' ), {
  cuid: cuid()
})

app.ports.saveTodosToPouch.subscribe(function (todos) {
   var todoDocs = JSON.parse(todos)
   todoDocs.forEach(todoDoc => {
     todosDB
       .upsert(todoDoc._id, (doc) => Object.assign({}, doc, todoDoc))
       .then(function (res) {
         app.ports.pouchSaveSuccess.send(JSON.stringify(res))
       }).catch(function (err) {
         app.ports.pouchSaveError.send(JSON.stringify(err))
         console.error(err)
      })
   })
})

app.ports.getTodosFromPouch.subscribe(function (filter) {
    todosDB.allDocs({include_docs: true}).then(function (result) {
      console.log('result', result)
      const todos = result.rows.map(row => row.doc)
      app.ports.pouchGetSuccess.send(JSON.stringify(todos))
    }).catch(function(err){
      console.error(err)
      app.ports.pouchGetError.send(err)
    })
})

setTimeout(function() {
  var input = document.getElementById('cm-command-input')
  input.addEventListener('keydown', function(e) {
    if (e.keyCode === 9) {
      e.preventDefault()
    }
  })
}, 0)
