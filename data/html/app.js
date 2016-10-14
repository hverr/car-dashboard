$(function() {
  var genAlert = _.template([
    '<div class="alert alert-danger alert-dismissable">',
    '  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>',
    '  <%= message %>',
    '</div>'
  ].join("\n"));

  function queryEngineData() {
    $.get('/api/engine/status').done(function(data) {
      console.log("got: " + JSON.stringify(data));
    });
  }

  function queryEngineErrors() {
    $.get('/api/engine/errors').done(function(data) {
      if (data.error != null) {
        showError(data.error);
      }
    }).always(function() {
      setTimeout(queryEngineErrors, 500);
    });
  }

  function showError(err) {
    $('#alert-box').append(genAlert({'message' : err}));
  }

  queryEngineData();
  queryEngineErrors();
});
