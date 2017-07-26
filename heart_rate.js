let chosenHeartRateService = null;

alert(navigator.bluetooth);
navigator.bluetooth.requestDevice({
  filters: [{
    services: ['heart_rate'],
  }]
}).then(device => device.gatt.connect())
.then(server => server.getPrimaryService('heart_rate'))
.then(service => {
  chosenHeartRateService = service;
  return Promise.all([
    service.getCharacteristic('body_sensor_location')
      .then(handleBodySensorLocationCharacteristic),
    service.getCharacteristic('heart_rate_measurement')
      .then(handleHeartRateMeasurementCharacteristic),
  ]);
});

function handleBodySensorLocationCharacteristic(characteristic) {
  if (characteristic === null) {
    console.log("Unknown sensor location.");
    return Promise.resolve();
  }
  return characteristic.readValue()
  .then(sensorLocationData => {
    let sensorLocation = sensorLocationData.getUint8(0);
    switch (sensorLocation) {
      case 0: return 'Other';
      case 1: return 'Chest';
      case 2: return 'Wrist';
      case 3: return 'Finger';
      case 4: return 'Hand';
      case 5: return 'Ear Lobe';
      case 6: return 'Foot';
      default: return 'Unknown';
    }
  }).then(location => console.log(location));
}

function handleHeartRateMeasurementCharacteristic(characteristic) {
  return characteristic.startNotifications()
  .then(char => {
    characteristic.addEventListener('characteristicvaluechanged',
                                    onHeartRateChanged);
  });
}

function onHeartRateChanged(event) {
  let characteristic = event.target;
  console.log(parseHeartRate(characteristic.value));
}