function log(){window.console && console.log.apply(console,arguments)}

$(document).ready(function(){
  setInterval(function(){
    $('#mixed tr').first().each(function(){
      var epoch = $(this).attr('id').replace(/[^0-9]/g,'');
      $.get('/after/' + epoch,function(html){
        if(html) {
          var last = $('#mixed-row');
          last.removeClass('#mixed-row');
          last.before(html);
        }
      });
    });
  },1000 * 60 * 5);
});