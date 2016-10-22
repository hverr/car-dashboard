Car Dashboard
=============

Use your Android phone as a car dashboard.

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
