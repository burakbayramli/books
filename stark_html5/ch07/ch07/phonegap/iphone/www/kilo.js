var jQT = $.jQTouch({
    formSelector: false,
    icon: 'icon.png',
    startupScreen: 'Default.png',
    statusBar: 'black', 
    useFastTouch: false,
    preloadImages: [
        'themes/jqt/img/back_button.png',
        'themes/jqt/img/back_button_clicked.png',
        'themes/jqt/img/button_clicked.png',
        'themes/jqt/img/grayButton.png',
        'themes/jqt/img/whiteButton.png',
        'themes/jqt/img/loading.gif'
    ]
});
var db;
$(document).ready(function(){
    if (typeof(PhoneGap) != 'undefined') {
        $('body > *').css({minHeight: '460px !important'});
    }
    // $('#about, #createEntry, #dates, #home, #settings').bind('touchmove', function(e){e.preventDefault()});
    $('#createEntry form').submit(createEntry);
    $('#editEntry form').submit(updateEntry);
    $('#settings form').submit(saveSettings);
    $('#date').bind('pageAnimationEnd', function(e, info){
        if (info.direction == 'in') {
            startWatchingShake();
        }
    });
    $('#date').bind('pageAnimationStart', function(e, info){
        if (info.direction == 'out') {
            stopWatchingShake();
        }
    });
    $('#settings').bind('pageAnimationStart', loadSettings);
    $('#dates li a').click(function(e){
        var dayOffset = this.id;
        var date = new Date();
        date.setDate(date.getDate() - dayOffset);
        sessionStorage.currentDate = date.getMonth() + 1 + '/' + date.getDate() + '/' + date.getFullYear();
        refreshEntries();
    });
    $('#share').click(function(e){
        try {
            navigator.ContactManager.chooseContact(function(contact){
                alert('got it');
            }, null);
        } catch(e) {
            alert('Contact Manager not available.');
        }
    });
    var shortName = 'Kilo';
    var version = '1.0';
    var displayName = 'Kilo';
    var maxSize = 65536;
    db = openDatabase(shortName, version, displayName, maxSize);
    db.transaction(
        function(transaction) {
            transaction.executeSql(
                'CREATE TABLE IF NOT EXISTS entries (' +
                'id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, ' + 
                'date DATE, food TEXT, calories INTEGER, ' + 
                'longitude TEXT, latitude TEXT);'
            );
        }
    );
});
function loadSettings() {
    $('#age').val(localStorage.age);
    $('#budget').val(localStorage.budget);
    $('#weight').val(localStorage.weight);
}
function saveSettings() {
    localStorage.age = $('#age').val();
    localStorage.budget = $('#budget').val();
    localStorage.weight = $('#weight').val();
    jQT.goBack();
    return false;
}
function createEntry() {
    try {
        navigator.geolocation.getCurrentPosition(
            function(position){
                console.log('geo success called');
                var latitude = position.coords.latitude;
                var longitude = position.coords.longitude;
                insertEntry(latitude, longitude);
            },
            function(){
                console.log('geo error called');
                insertEntry();
            } 
        );
    } catch(e) {
        console.log('Catch called in createEntry');
        insertEntry();
    }
    return false;
}
function insertEntry(latitude, longitude) {
    var date = sessionStorage.currentDate;
    var calories = $('#calories').val();
    var food = $('#food').val();
    db.transaction(
        function(transaction) {
            transaction.executeSql(
                'INSERT INTO entries (date, calories, food, latitude, longitude) VALUES (?, ?, ?, ?, ?);', 
                [date, calories, food, latitude, longitude], 
                function(){
                    refreshEntries();
                    checkBudget();
                    jQT.goBack();
                }, 
                errorHandler
            );
        }
    );
}
function refreshEntries() {
    var currentDate = sessionStorage.currentDate;
    $('#date h1').text(currentDate);
    $('#date ul li:gt(0)').remove();
    db.transaction(
        function(transaction) {
            transaction.executeSql(
                'SELECT * FROM entries WHERE date = ? ORDER BY food;', 
                [currentDate], 
                function (transaction, result) {
                    for (var i=0; i < result.rows.length; i++) {
                        var row = result.rows.item(i);
                        var newEntryRow = $('#entryTemplate').clone();
                        newEntryRow.removeAttr('id');
                        newEntryRow.removeAttr('style');
                        newEntryRow.data('entryId', row.id);
                        newEntryRow.appendTo('#date ul');
                        newEntryRow.find('.label').text(row.food);
                        newEntryRow.find('.calories').text(row.calories);
                        newEntryRow.find('.delete').click(function(e){
                            var clickedEntry = $(this).parent();
                            var clickedEntryId = clickedEntry.data('entryId');
                            deleteEntryById(clickedEntryId);
                            clickedEntry.slideUp();
                            e.stopPropagation();
                        });
                        newEntryRow.click(entryClickHandler);
                    }
                }, 
                errorHandler
            );
        }
    );
}
function deleteEntryById(id) {
    db.transaction(
        function(transaction) {
            transaction.executeSql('DELETE FROM entries WHERE id=?;', [id], null, errorHandler);
        }
    );
}
function errorHandler(transaction, error) {
    var message = 'Oops. Error was: "'+error.message+'" (Code '+error.code+')';
    try {
        navigator.notification.alert(message, 'Error', 'Dang!');
    } catch(e) {
        alert(message);
    }
    return true;
}
function checkBudget() {
    var currentDate = sessionStorage.currentDate;
    var dailyBudget = localStorage.budget || 2000;
    db.transaction(
        function(transaction) {
            transaction.executeSql(
                'SELECT SUM(calories) AS currentTotal FROM entries WHERE date = ?;', 
                [currentDate], 
                function (transaction, result) {
                    var currentTotal = result.rows.item(0).currentTotal;
                    if (currentTotal > dailyBudget) {
                        var overage = currentTotal - dailyBudget;
                        var message = 'You are '+overage+' calories over your daily budget. Better start jogging!';
                        try {
                            navigator.notification.beep();
                            navigator.notification.vibrate();
                        } catch(e){
                            // No equivalent in web app
                        }
                        try {
                            navigator.notification.alert(message, 'Over Budget', 'Dang!');
                        } catch(e) {
                            alert(message);
                        }
                    }
                }, 
                errorHandler
            );
        }
    );
}
function updateEntry() {
    var date = sessionStorage.currentDate;
    var calories = $('#editEntry input[name="calories"]').val();
    var food = $('#editEntry input[name="food"]').val();
    var latitude = $('#editEntry input[name="latitude"]').val();
    var longitude = $('#editEntry input[name="longitude"]').val();
    var id = sessionStorage.entryId;
    db.transaction(
        function(transaction) {
            transaction.executeSql(
                'UPDATE entries SET date=?, calories=?, food=?, latitude=?, longitude=? WHERE id=?;', 
                [date, calories, food, latitude, longitude, id], 
                function(){
                    refreshEntries();
                    checkBudget();
                    jQT.goBack();
                }, 
                errorHandler
            );
        }
    );
    return false;
}

function entryClickHandler(e){
    sessionStorage.entryId = $(this).data('entryId');
    db.transaction(
        function(transaction) {
            transaction.executeSql(
                'SELECT * FROM entries WHERE id = ?;', 
                [sessionStorage.entryId], 
                function (transaction, result) {
                    var row = result.rows.item(0);
                    var food = row.food;
                    var calories = row.calories;
                    var latitude = row.latitude;
                    var longitude = row.longitude;
                    $('#editEntry input[name="food"]').val(food);
                    $('#editEntry input[name="calories"]').val(calories);
                    $('#editEntry input[name="latitude"]').val(latitude);
                    $('#editEntry input[name="longitude"]').val(longitude);
                    $('#saveChanges').click(function(){
                        // alert('submitted');
                        // $('#editEntry form').submit();
                        updateEntry();
                    });
                    $('#mapLocation').click(function(){
                        window.location = 'http://maps.google.com/maps?z=15&q='+food+'@'+latitude+','+longitude;
                    });
                    jQT.goTo('#editEntry', 'slideup');
                }, 
                errorHandler
            );
        }
    );
}
function dupeEntryById(entryId) {
    console.log('dupeEntryById called with id: ' + entryId);
    if (entryId == undefined) {
        console.log('You have to have at least one entry in the list to shake out a dupe.');
    } else {
        db.transaction(
            function(transaction) {
                transaction.executeSql(
                    'INSERT INTO entries (date, food, calories, latitude, longitude) SELECT date, food, calories, latitude, longitude FROM entries WHERE id = ?;', 
                    [entryId], 
                    function () {
                        console.log('Success called.');
                        refreshEntries();
                    }, 
                    errorHandler
                );
            }
        );
    }
}
function startWatchingShake() {
    try {
        debug.log('startWatchingShake called');
        var success = function(coords){
            var max = 2;
            if (Math.abs(coords.x) > max || Math.abs(coords.y) > max || Math.abs(coords.z) > max) {
                debug.log('dupe called');
                var entryId = $('#date ul li:last').data('entryId');
                dupeEntryById(entryId);
            }
        };
        var error = function(){};
        var options = {};
        options.frequency = 100;
        sessionStorage.watchId = navigator.accelerometer.watchAcceleration(success, error, options);
    } catch(e) {
        console.log('Catch called in startWatchingShake');
    }
}
function stopWatchingShake() {
    try {
        debug.log('stopWatchingShake called');
        navigator.accelerometer.clearWatch(sessionStorage.watchId);
    } catch(e) {
        console.log('Catch called in stopWatchingShake');
    }
}
