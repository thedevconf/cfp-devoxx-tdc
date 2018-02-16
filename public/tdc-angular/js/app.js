'use strict';


// Declare app level module which depends on filters, and services
var cfpApp = angular.module('cfpApp', [
    'ngRoute',
    'ngResource',
    'remoteServices',
    'mainController',
    'adminController',
    'flash'
]);

cfpApp.config(['$routeProvider', function ($routeProvider) {
    $routeProvider.when('/slots', {
        templateUrl: '/assets/tdc-angular/partials/slots.html',
        controller: 'MainController'
    });
    $routeProvider.when('/saved', {
        templateUrl: '/assets/tdc-angular/partials/savedSlots.html',
        controller: 'AdminController'
    });
    $routeProvider.when('/slots/:id', {
        templateUrl: '/assets/tdc-angular/partials/slots.html',
        controller: 'MainController'
    });
    $routeProvider.otherwise({redirectTo: '/'});
}]);

