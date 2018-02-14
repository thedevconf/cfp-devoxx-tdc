'use strict';

/* Services */
//
var remoteServices = angular.module('remoteServices', ['ngResource']);

remoteServices.factory('ApprovedTalksService', function($resource) {
   return $resource('/cfpadmin/scheduling/approvedTalks');
});

remoteServices.factory('SlotService', function($resource) {
   return $resource('/cfpadmin/scheduling/slots');
});

/*
remoteServices.factory('AllScheduledConfiguration', ['$resource', function($resource){
    return $resource('/schedulling/scheduledConfigurations', null, {'query': {method: 'GET', isArray: true, responseType:'json'}})
}]);

remoteServices.factory('ReloadScheduleConf', ['$resource', function($resource){
    return $resource('/schedulling/loadScheduledConfiguration', null, {'query': {method: 'GET', isArray: false, responseType:'json'}})
}]);

remoteServices.factory('DeleteScheduledConfiguration', ['$resource', function($resource){
  return $resource('/schedulling/deletescheduledConfigurations', null, {'query': {method: 'DELETE', isArray: false, responseType:'json'}})
}]);

remoteServices.factory('PublishScheduledConfiguration', ['$resource', function($resource){
  return $resource('/schedulling/publish', null, {'save': { method:'POST'}});
}]);
*/