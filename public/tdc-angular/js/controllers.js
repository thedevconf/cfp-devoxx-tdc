'use strict';

/* Controllers */
var mainController = angular.module('mainController', []);
var adminController = angular.module('adminController', []);

adminController.controller('AdminController', function AdminController($rootScope, $scope, $routeParams, ScheduleService, flash) {
    ScheduleService.get(function(jsonArray){
       $scope.allScheduledTracks = jsonArray["scheduledTracks"];
    });
    $scope.deleteSchedule = function(track) {
        ScheduleService.delete({id:track.id},function(jsonObj) {
            flash("Schedule removed for " + track.label)  // Programação removida para
            ScheduleService.get(function(jsonArray){
                           $scope.allScheduledTracks = jsonArray["scheduledTracks"];
            });
        })
    }
    $scope.toggleBlockedStatus = function(track) {
        var trackToChange = $scope.allScheduledTracks.find(t => t.id === track.id);
        trackToChange.blocked = !trackToChange.blocked;
        ScheduleService.updateStatus({id:track.id}, trackToChange);
    }
});


mainController.controller('MainController', function MainController($rootScope, $scope, $routeParams, ScheduleService, PublishingService, NotificationService, flash) {

    ScheduleService.get({id: $routeParams.id}, function (jsonObj) {
        // Left column, list of accepted proposal
        $scope.approvedTalks =  jsonObj["approvedTalks"];
        //Right column, list of slots
        if(_.isUndefined(jsonObj["fullSchedule"])) {
            $scope.slots = [{id:'1',proposals:[]}
                       ,{id:'2',proposals:[]}
                       ,{id:'3',proposals:[]}
                       ,{id:'4',proposals:[]}
                       ,{id:'5',proposals:[]}
                       ,{id:'6',proposals:[]}
                       ,{id:'7',proposals:[]}]
        } else {
            $scope.slots = jsonObj["fullSchedule"].slots;
        }
        $scope.blocked = jsonObj["fullSchedule"].blocked

    }, function(error) {
          $scope.disableButtons = true
		  flash("error","Unauthorized")
    });

    $rootScope.$on('dropEvent', function (evt, dragged, dropped) {

        var maybeSlot = _.find($scope.slots, function (slot) {
            return slot.id == dropped.id;
        });
        if (_.isUndefined(maybeSlot)) {
            console.log("old slot not found");
        } else {

            // Update the slot
            maybeSlot.proposals.push(dragged);

            // remove from accepted talks
            $scope.approvedTalks = _.reject($scope.approvedTalks, function (a) {
                return a.id === dragged.id
            });
            $scope.messages = []

            $scope.$apply();
        }
    });

    $scope.deallocate = function(slotId,position){
       var maybeSlot = _.find($scope.slots, function (slot) {
            return slot.id == slotId;
        });
        if (_.isUndefined(maybeSlot)) {
            console.log("old slot not found");
        } else {
            var talk=maybeSlot.proposals[position] ;

            // Remove from left
            maybeSlot.proposals.splice(position,1);

            // Add back to right
            $scope.approvedTalks = $scope.approvedTalks.concat(talk);
            $scope.messages = []
        }
    };

    $scope.saveAllocation = function() {
        var jsonSlots = $scope.slots.map(slot => {
            return {id: slot.id, proposals: slot.proposals.map(proposal => proposal.id), stadium: slot.stadium};
        });
        ScheduleService.save({trackId: $routeParams.id}, jsonSlots);
        flash("Schedule saved"); // Programação Salva

    };

    $scope.toggleStadium = function(slotId) {
        $scope.slots.forEach(slot => {
            if(slot.id !== slotId) {
                slot.stadium = false;
            } else {
                slot.stadium = !slot.stadium
            }
        });
    };

    $scope.requestPublication = function() {
        PublishingService.save({trackId: $routeParams.id});
        flash("Request sent");  // Solicitação enviada
    }

    $scope.requestUnlocking = function() {
        NotificationService.save({trackId: $routeParams.id});
        flash("Request sent"); // Solicitação enviada
    }	
	
});