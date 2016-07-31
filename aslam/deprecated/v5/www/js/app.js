var _ = {};

/* Config */

_.config = {
  url: 'ws://127.0.0.1:8080',
  colors: {
    sub: '#000000',
    buoy_a: '#FF0000',
    buoy_b: '#0000FF',
    buoy_c: '#00FF00',
    buoy_d: '#FFFF00'
  },
  mode: '3D',
  origin: [0, 0, 0]
};

_.stats = {
    version: '0.8.0',
    ram: '0 MB',
    cpu: '0 %'
}; 

_.objects = {};

/* Status */

var header = document.createElement('div');
header.style.position = 'absolute';
header.style.zIndex = 1;
header.style.height = '100px';
header.style.width = '500px';
header.style.top = '10px';
header.style.left = ((window.innerWidth / 2) - 250) + 'px';
header.innerHTML = 'HEADER';
document.body.appendChild(header);

var stats = document.createElement('div');
stats.style.position = 'absolute';
stats.style.color = '#FFFFFF';
stats.style['font-size'] = '11px';
stats.style.zIndex = 1;
stats.style.height = '100px';
stats.style.width = '500px';
stats.style.top = '10px';
stats.style.left = '20px';
document.body.appendChild(stats);

var setStats = function() {
  stats.innerHTML = '<strong>ASLAM</strong> v' + _.stats.version + ' | Remote ' + _.config.url + ' | CPU ' + _.stats.cpu + ' | RAM ' + _.stats.ram;
};

setStats();

var button = function(innerHTML, onClick) {
  var b = document.createElement('div');
  b.style.background = '#000000';
  b.style.color = '#FFFFFF';
  b.style.zIndex = 1;
  b.innerHTML = innerHTML;
  b.style.top = '10px';
  b.style.padding = '5px';
  b.style['margin-left'] = '5px';
  b.style['margin-right'] = '5px';
  b.style.outline = '1px #FFFFFF solid';
  b.style.display = 'inline-block';
  b.style.cursor = 'pointer';
  b.onclick = onClick;
  return b;
};

/* WS */

_.log = console.log;

_.compress = function(s) { return pako.deflate(s, {from: 'string'}); };
_.decompress = function(s) { return pako.inflate(s, {to: 'string'}); };

_.connect = function() {
  _.sock = new WebSocket(_.config.url);
  _.sock.binaryType = 'arraybuffer';
  _.sock.onopen = function () {
    _.send = function(m) {
      _.sock.send(_.compress(JSON.stringify(m)));
    };  
    _.sock.onmessage = function(m) {
      m = JSON.parse(_.decompress(m.data));
      _.objects[m.obj] = m;
      updatePoints();
    };  
    _.sock.onclose = function() {
      _.log('CLOSED!');
    };  
    _.sock.onerror = function() {
      _.log(e);
    };  
  };  
};

_.connect();

_.points = {};

/* Three */

var scene = new THREE.Scene();
// var camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);

var width = window.innerWidth;
var height = window.innerHeight;

var camera = new THREE.PerspectiveCamera(75, window.innerWidth / window.innerHeight, 0.1, 1000);
// var camera = new THREE.OrthographicCamera(width / - 2, width / 2, height / 2, height / - 2, 1, 1000);
var renderer = new THREE.WebGLRenderer();

var axis = function(src, dst) {
  var g = new THREE.Geometry();
  var m = new THREE.LineBasicMaterial({color: 0xffffff});
  g.vertices.push(src.clone());
  g.vertices.push(dst.clone());
  var a = new THREE.Line(g, m, THREE.LineSegments);
  return a;
};

var controls = new THREE.OrbitControls(camera, renderer.domElement);
var geometry = new THREE.SphereGeometry(0.05, 1, 1);

/* Thanks SO (http://stackoverflow.com/questions/5623838/rgb-to-hex-and-hex-to-rgb) */
var hexToRgb = function(hex) {
    var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    return result ? {
        r: parseInt(result[1], 16),
        g: parseInt(result[2], 16),
        b: parseInt(result[3], 16)
    } : null;
}

var updatePoints = function() {
  var points = Object.keys(_.objects).map(function(k) { 
    const color = hexToRgb(_.config.colors[k]);
    return _.objects[k].space[Object.keys(_.objects[k].space)[0]].map(function(p) { 
      const rat = p[1] * 255;
      return {x: p[0][0], y: p[0][1], z: p[0][2], c: {r: color.r * rat, g: color.g * rat, b: color.b * rat}};
    });
  });
  points = [].concat.apply([], points);
  var ind = points.length;
  scene.children = [];
  for (var i = 0; i < ind; i++) {
    var point = new THREE.Mesh(geometry, new THREE.MeshBasicMaterial());
    point.position.x = points[i].x;
    /* y/z swapped from CUAUV reference frame to three.js */
    point.position.y = _.config.mode == '2D' ? 0 : -points[i].z;
    point.position.z = points[i].y;
    point.material.color = points[i].c;
    /*
    point.translateX(_.config.origin[0]);
    point.translateY(_.config.origin[1]);
    point.translateZ(_.config.origin[2]);
    */ 
    scene.add(point);
  };
  var axes = new THREE.Object3D();
  const l = 50;
  axes.add(axis(new THREE.Vector3(0, 0, 0), new THREE.Vector3(l, 0, 0)));
  axes.add(axis(new THREE.Vector3(0, 0, 0), new THREE.Vector3(-l, 0, 0)));
  axes.add(axis(new THREE.Vector3(0, 0, 0), new THREE.Vector3(0, l, 0)));
  axes.add(axis(new THREE.Vector3(0, 0, 0), new THREE.Vector3(0, -l, 0)));
  axes.add(axis(new THREE.Vector3(0, 0, 0), new THREE.Vector3(0, 0, l)));
  axes.add(axis(new THREE.Vector3(0, 0, 0), new THREE.Vector3(0, 0, -l)));
  axes.translateX(_.config.origin[0]);
  axes.translateY(_.config.origin[1]);
  axes.translateZ(_.config.origin[2]);
  scene.add(axes);
};

var swap3DGrid = function() {
  _.config.mode = '3D';
  _.config.origin = [0, 0, 0];
};
var swap2DProj = function() {
  _.config.mode = '2D';
  _.config.origin = [0, 0, 0];
};
var swapTrackFOV = function() {
  _.config.mode = 'TRACK';
  _.config.origin = [0, 0, 0];
};

header.appendChild(button('3D Grid', swap3DGrid));
header.appendChild(button('2D Projection', swap2DProj));
header.appendChild(button('Track FOV', swapTrackFOV));

renderer.setSize(window.innerWidth, window.innerHeight);
document.body.appendChild(renderer.domElement);

camera.position.z = 10;
camera.position.x = 0;
camera.position.y = 0;

camera.lookAt(new THREE.Vector3(10, 10, 0));

var stats = new Stats();
stats.setMode(0);
stats.domElement.style.position = 'absolute';
stats.domElement.style.right = '0px';
stats.domElement.style.top = '0px';

document.body.appendChild(stats.domElement);

var render = function() {
  stats.begin();
  switch(_.config.mode) {
    case '3D':
      break;
    case '2D': 
      break;
    case 'TRACK':
      _.config.origin[0] = _.objects.sub.est[0];
      _.config.origin[1] = -_.objects.sub.est[2];
      _.config.origin[2] = _.objects.sub.est[1];
      camera.position.x = _.objects.sub.est[0];
      camera.position.y = -_.objects.sub.est[2];
      camera.position.z = _.objects.sub.est[1];
      camera.lookAt(new THREE.Vector3(_.objects.sub.est[0] + _.objects.sub.orientation[0],
                                      -_.objects.sub.est[2] - _.objects.sub.orientation[2],
                                      _.objects.sub.est[1] + _.objects.sub.orientation[1]));
  };
  renderer.render(scene, camera);
  stats.end();
  setStats();
  requestAnimationFrame(render);
}

render();
