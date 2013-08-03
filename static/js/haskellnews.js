/*******************************************************************************
 * Model operations
 */

function getNewItems(){
  $('tr').first().each(function(){
    var epoch = $(this).attr('id').replace(/[^0-9]/g,'');
    $.get('/after/' + epoch,function(html){
      if(html) {
        $('table').prepend(html);
      }
    });
  });
}

function refreshDates(){
  $('.relative-time').each(function(){
    var t = (new Date($(this).attr('data-epoch') * 1000));
    var now = new Date();
    $(this).text(t.relative(now,true));
  });
}

/*******************************************************************************
 *  Date utilities
 */

Date.prototype.relative = function(t2,fix){
  var t1 = this;
  var diff = t1 - t2;
  var minute = 60, hour = minute * 60, day = hour * 24,
  week = day * 7, month = day * 30, year = month * 12;
  return inRange(
    [0,'just now'],
    [5,'% seconds',1],
    [minute,'a minute'],
    [minute*2,'% minutes',minute],
    [minute*30,'half an hour'],
    [minute*31,'% minutes',minute],
    [hour,'an hour'],
    [hour*2,'% hours',hour],
    [hour*3,'a few hours'],
    [hour*4,'% hours',hour],
    [day,'a day'],
    [day*2,'% days',day],
    [week,'a week'],
    [week*2,'% weeks',week],
    [month,'a month'],
    [month*2,'% months',month],
    [year,'a year'],
    [year*2,'% years',year]
  );
  function inRange() {
    var span = Math.abs(diff/1000);
    for (var i = arguments.length-1; i >= 0; i--) {
      var range = arguments[i];
      if (span >= range[0]) {
        return (
          (fix&& diff>0?'in ':'') +
            (range[1].match(/%/)?
             range[1].replace(/%/g,Math.round(span/(range[2]? range[2] : 1)))
             : range[1]) +
            (fix&& diff<0?' ago':'')
        );
      }
    }
  }
};

/*******************************************************************************
 * Main entry point
 */

$(document).ready(function(){
  refreshDates();
  setInterval(getNewItems,1000 * 60 * 5);
  setInterval(refreshDates,1000);
});