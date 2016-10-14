$(function() {
  var genAlert = _.template([
    '<div class="alert alert-danger alert-dismissable">',
    '  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>',
    '  <%= message %>',
    '</div>'
  ].join("\n"));

  function queryEngineData() {
    $.get('/api/engine/status').done(function(data) {
      updateValue("speed", data.vehicleSpeed, function(v) {
        $('#vehicle-speed-input').val(v).trigger('change');
      });
      updateValue("engine RPM", data.engineRPM, function(v) {
        $('#engine-rpm-input').val(v).trigger('change');
      });
      updateValue("fuel rate", data.engineFuelRate, function(v) {
        $('#fuel-rate-value2').text(v);

        if(data.vehicleSpeed.value) {
          var x = v * 100 / data.vehicleSpeed.value;
          var w = (x / 15 > 1) ? 100 : x * 100 / 15;
          $('#fuel-rate-bar').attr('style', 'width: ' + w + '%;');
          $('#fuel-rate-value').text(x);
        } else {
          $('#fuel-rate-bar').attr('style', 'width: 0%;');
          $('#fuel-rate-value').html('&infin;');
        }
      });
      updateValue("coolant temperature", data.engineCoolantTemperature, function(v) {
        var w = (v / 180 > 1) ? 100 : v * 100 / 180;
        $('#coolant-temp-value').text(v);
        $('#coolant-temp-bar').attr('style', 'width: ' + w + '%;');
      });
      updateValue("intake air temperature", data.intakeAirTemperature, function(v) {
        $('#intake-air-temp-value').text(v);
      });
      updateValue("throttle position", data.throttlePosition, function(v) {
        $('#throttle-position-value').text(v);
        $('#throttle-position-bar').attr('style', 'width: ' + v + '%;');
      });
    }).always(function() {
      setTimeout(queryEngineData, 250);
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

  function updateValue(humanName, data, f) {
    if (data.error !== null) {
      showError("Could not get " + humanName + ": " + value.error);
      return;
    }
    f(data.value);
  }

  function showError(err) {
    $('#alert-box').append(genAlert({'message' : err}));
  }

  queryEngineData();
  queryEngineErrors();
});
