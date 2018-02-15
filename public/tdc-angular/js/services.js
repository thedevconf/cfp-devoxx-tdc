'use strict';

/* Services */
//
var remoteServices = angular.module('remoteServices', ['ngResource']);

remoteServices.factory('ScheduleService', function($resource){
    return $resource('/cfpadmin/scheduling/schedules/:id',{id:"@id"});
});

