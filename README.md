Car Dashboard
=============

Use your Android phone as a car dashboard using a Raspberry Pi, to

 - Monitor speed, RPM, fuel rate, engine temperature, ...
 - Stream music from your phone to FM on 87.5 kHz

Installing
----------

1. Download the latest release
2. Install the deb file using `dpkg -i`
3. Possibly edit `/etc/systemd/system/car-dashboard.service` to select the correct OBD protocol
4. Run `systemctl daemon-reload; systemctl start car-dashboard.service'`
5. Install the [Android application](https://github.com/hverr/car-dashboard-android)
6. Turn on the WiFi hotspot on your phone
7. Connect your Raspberry Pi to your phone's WiFi network with static IP 192.168.43.65
8. Open the Android application and enable music streaming
9. Point your FM car radio to 87.5 kHz

![screenshot of dashboard][dashboard] ![screenshot of streaming][streaming]

  [dashboard]: media/dashboard_250.png
  [streaming]: media/streaming_250.png

Developing
----------

```sh
sudo apt-get install -y npm
make setup
make js
stack build
stack exec car-dashboard -- --simulator
```

Now point your browser to `http://127.0.0.1:8080`.


Deployment
----------

The following steps are needed to deploy the application to a Raspberry Pi.

```sh
./package.sh
dpkg -i dist/car-dashboard-*.deb
systemctl daemon-reload
systemctl enable car-dashboard.service
systemctl start car-dashboard.service
```
