#!/usr/bin/env python2
import shm,cgi , cgitb
cgitb.enable()

depth = shm.depth.depth.get()
pressure = shm.pressure.internal.get()
voltage = shm.pod.voltage.get()
pVolts = shm.pod.voltage_port.get()
sVolts = shm.pod.voltage_starboard.get()
current = shm.pod.current.get()
currentPort = shm.pod.current_port.get()
currentStarboard = shm.pod.current_starboard.get()
aftPortStat = shm.motor_status.status_aft_port.get()
aftStarboardStat = shm.motor_status.status_aft_starboard.get()
forePortStat = shm.motor_status.status_fore_port.get()
foreStarboardStat = shm.motor_status.status_fore_starboard.get()
portStat = shm.motor_status.status_port.get()
starboardStat = shm.motor_status.status_starboard.get()
swayAftStat = shm.motor_status.status_sway_aft.get()
swayForeStat = shm.motor_status.status_sway_fore.get()

print """Content-type: text/html
  <head>
    <meta charset="utf-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <!-- The above 3 meta tags *must* come first in the head; any other head content must come *after* these tags -->
    <meta name="description" content="">
    <meta name="author" content="">
    <!--link rel="icon" href="../../favicon.ico"-->

    <title>Argo Status</title>

    <!-- Bootstrap core CSS -->
    <link href="css/bootstrap.min.css" rel="stylesheet">

    <!-- Custom styles for this template -->
    <link href="css/jumbotron.css" rel="stylesheet">

    <script src="./ie-emulation-modes-warning.js"></script>

    <!-- HTML5 shim and Respond.js for IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="https://oss.maxcdn.com/html5shiv/3.7.2/html5shiv.min.js"></script>
      <script src="https://oss.maxcdn.com/respond/1.4.2/respond.min.js"></script>
    <![endif]-->
  </head>

<body>"""
print """
    <nav class="navbar navbar-inverse navbar-fixed-top">
      <div class="container">
        <div class="navbar-header">
          <a class="navbar-brand" href="#">Argo Info</a>
        </div>
        <div id="navbar" class="navbar-collapse collapse">
        </div><!--/.navbar-collapse -->
      </div>
    </nav>
<div class="jumbotron">
    <div class ="container">
    <h1>Information about Argo</h1>
    <p> This page is intended to help team members understand more about the current status of Argo</p>
    </div>
</div>
<div class="container">
    <div class="row">
        <div class="col-md-4">
            <h2>Vital Info </h2>
            <p>Depth: {depth} m</p>
            <p>Pressure: {pressure} psi</p>
        </div>
        <div class="col-md-4">
            <h2>Voltage</h2>
            <p>Voltage: {voltage} Volts</p>
            <p>Voltage Port: {pVolts} Volts</p>
            <p>Voltage Starboard: {sVolts} Volts</p>
        </div>
        <div class="col-md-4">
            <h2>Current</h2>
            <p>Current: {current} Amps</p>
            <p>Current Port: {currentPort} Amps</p>
            <p>Current Starboard: {currentStarboard} Amps</p>
        </div>
    </div> <!-- end row -->
</div> <!-- end container -->
<div class="container">
    <div class="row">
        <div class="col-md-4">
            <h2>Motor Status</h2>
            <div>
                <p> The status numbers indicate 0: no connection, 1: ok, 2: invalid device </p>
                <p>aft port: {aftPortStat}</p>
                <p>aft starboard: {aftStarboardStat}</p>
                <p>fore port: {forePortStat}</p>
                <p>fore starboard: {foreStarboardStat}</p>
                <p>port: {portStat}</p>
                <p>starboard: {starboardStat}</p>
                <p>sway aft: {swayAftStat}</p>
                <p>sway fore: {swayForeStat}</p>
            </div>
        </div> <!-- end col -->
    </div> <!-- end row -->
</div> <!-- end second container -->
<div class="col-md-4">
    <h2>Serial Ports</h2>
""".format(depth=depth,pressure=pressure,voltage=voltage,pVolts=pVolts, sVolts=sVolts,\
        current=current, currentPort=currentPort, currentStarboard=currentStarboard,\
        aftPortStat=aftPortStat,aftStarboardStat=aftStarboardStat, forePortStat=forePortStat,\
        foreStarboardStat=foreStarboardStat, portStat=portStat, starboardStat=starboardStat,\
        swayAftStat=swayAftStat, swayForeStat= swayForeStat)
f = open ('serialstatus.txt','r+')
for line in f.readlines():
    print "<p>"+line+"</p>"
f.close()
print "</div>"
print "</body>"
