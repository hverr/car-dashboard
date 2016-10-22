$(function() {
  interface P {
    value: number;
    dim: string;
  }

  interface CarData {
    dateTimestamp: string;
    engineCoolantTemperature: P;
    engineFuelRate: P;
    engineRPM: P;
    intakeAirTemperature: P;
    intakeManifoldAbsolutePressure: P;
    massAirFlowRate: P;
    throttlePosition: P;
    vehicleSpeed: P;
  }

  interface Error {
    error: string;
  }

  var genAlert = _.template([
    '<div class="alert alert-danger alert-dismissable">',
    '  <button type="button" class="close" data-dismiss="alert" aria-hidden="true">&times;</button>',
    '  <%= message %>',
    '</div>'
  ].join("\n"));

  function queryEngineData(): void {
    $.get('/api/engine/status').done(function(data: CarData) {
      $('#vehicle-speed-input').val(f(data.vehicleSpeed.value, 0)).trigger('change');
      $('#engine-rpm-input').val(f(data.engineRPM.value, 0)).trigger('change');

      $('#fuel-rate-value2').text(f(data.engineFuelRate.value, 2));

      if(data.vehicleSpeed.value) {
        var v = data.engineFuelRate.value;
        var x = v * 100 / data.vehicleSpeed.value;
        var w = (x / 15 > 1) ? 100 : x * 100 / 15;
        $('#fuel-rate-bar').attr('style', 'width: ' + w + '%;');
        $('#fuel-rate-value').text(f(x, 2));
      } else {
        $('#fuel-rate-bar').attr('style', 'width: 0%;');
        $('#fuel-rate-value').html('&infin;');
      }

      (function() {
        var v = data.engineCoolantTemperature.value;
        var w = (v / 180 > 1) ? 100 : v * 100 / 180;
        $('#coolant-temp-value').text(f(v, 0));
        $('#coolant-temp-bar').attr('style', 'width: ' + w + '%;');
      })();

      $('#intake-air-temp-value').text(f(data.intakeAirTemperature.value, 0));

      (function() {
        var v = data.throttlePosition.value * 100;
        $('#throttle-position-value').text(f(v, 0));
        $('#throttle-position-bar').attr('style', 'width: ' + v + '%;');
      })();
    }).always(function() {
      setTimeout(queryEngineData, 250);
    });
  }

  function queryEngineErrors(): void {
    $.get('/api/engine/errors').done(function(data: Error) {
      if (data.error != null) {
        showError(data.error);
      }
    }).always(function() {
      setTimeout(queryEngineErrors, 500);
    });
  }

  function showError(err: string): void {
    $('#alert-box').append(genAlert({'message' : err}));
  }

  function f(v: number, p: number): string {
    return v.toFixed(p);
  }

  queryEngineData();
  queryEngineErrors();
});
