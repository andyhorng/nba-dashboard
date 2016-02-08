
angular.module('nbaApp', [])
  .controller('ListController', function() {
    var socket = io();

    var ctrl = this;

    ctrl.totalSet = [];
    ctrl.moneyLineSet = [];
    ctrl.spreadSet = [];

    socket.on('nba_update', function (data) {
      $scope.apply(function() {
        console.log(data);
        // TODO
        ctrl.totalSet = [""];
      });
    });

  });


// TODO: mutation observer
