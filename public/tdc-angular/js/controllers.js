'use strict';

/* Controllers */
var mainController = angular.module('mainController', []);
/*var homeController = angular.module('homeController', []);
var reloadScheduleConfController = angular.module('reloadScheduleConfController', []);
var deleteSlotController = angular.module('deleteSlotController', []);
var publishController = angular.module('publishController', []);*/

/*
homeController.controller('HomeController', function HomeController($rootScope, $scope, $routeParams, AllScheduledConfiguration) {
    AllScheduledConfiguration.get(function(jsonArray){
       $scope.allScheduledConfiguration = jsonArray["scheduledConfigurations"];
    });
});
*/

mainController.controller('MainController', function MainController($rootScope, $scope, $routeParams, ApprovedTalksService, flash) {
    // Left column, list of accepted proposal
    ApprovedTalksService.get({trackId: $routeParams.trackId}, function (allApproved) {
        // If a ScheduleConfiguration was reloaded, then we need to filter-out the list of ApprovedTalks
        if (_.isUndefined($rootScope.slots) == false) {
            var onlyValidProposals = _.reject($rootScope.slots, function(slot){ return _.isUndefined(slot.proposal)} );
            var onlyIDs=  _.map(onlyValidProposals, function(slot){return slot.proposal.id; });

            var filteredTalks = _.reject(allApproved["approvedTalks"].talks, function(talk){
                return _.contains(onlyIDs,talk.id);
            });
            $scope.approvedTalks=filteredTalks;
        } else {
            console.log("No schedule configuration loaded");
            $scope.approvedTalks =  allApproved["approvedTalks"].talks ;
        }
    });

    //Right column, list of slots
    $scope.slots = [{id:1,proposals:[]}
                    ,{id:2,proposals:[]}
                    ,{id:3,proposals:[]}
                    ,{id:4,proposals:[]}
                    ,{id:5,proposals:[]}
                    ,{id:6,proposals:[]}
                    ,{id:7,proposals:[]}]

    // Load a schedule configuration
 /*   SlotService.get({trackId: $routeParams.trackId}, function (jsonArray) {
        $scope.slots = jsonArray["allSlots"];

        // If we selected a ScheduleConfiguration to reload, then do not merge and update slots
        if (_.isUndefined($rootScope.slots) == false) {
            _.each($scope.slots, function (initialSlot) {
                var maybeSlot2 = _.find($rootScope.slots, function (slot2) {
                    return slot2.id == initialSlot.id;
                });
                if (_.isUndefined(maybeSlot2) == false) {
                    if (_.isUndefined(maybeSlot2.proposal) == false) {
                        initialSlot.proposal = maybeSlot2.proposal;
                    }
                }
            });
        } else {
            console.log("No schedule configuration loaded");
        }
    });*/


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
        }
    };

    $scope.saveAllocation=function(){
        flash("Allocation for "+$routeParams.confType+" saved");
        SlotService.save({confType: $routeParams.confType}, $scope.slots);
    };

    $scope.isNotAcepted=true;
});
/*
reloadScheduleConfController.controller('ReloadScheduleConfController', function ReloadScheduleConfController($location, $rootScope, $scope, $routeParams, ReloadScheduleConf) {
    ReloadScheduleConf.get({id: $routeParams.id}, function (jsonObj){
       $scope.loadedScheduledConfiguration = jsonObj;
        if (_.isUndefined($scope.loadedScheduledConfiguration)) {
            console.log("ERR: conf type not found");
        } else {
            var newConfType = $scope.loadedScheduledConfiguration.confType;
            $rootScope.slots = $scope.loadedScheduledConfiguration.slots;
            $location.path('/slots').search({confType: newConfType}).replace();
        }
    });

});

deleteSlotController.controller('DeleteSlotController', function DeleteSlotController($routeParams,$location, DeleteScheduledConfiguration,flash ){
    DeleteScheduledConfiguration.delete({id: $routeParams.id}, function (jsonObj){
        flash("Deleted configuration");
    });
});

publishController.controller('PublishController', function PublishController($routeParams,$location, PublishScheduledConfiguration, flash ){
    PublishScheduledConfiguration.save({id: $routeParams.id, confType: $routeParams.confType}, function (jsonObj){
        flash("Configuration published");
    });
});
*/