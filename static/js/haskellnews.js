function log(){window.console && console.log.apply(console,arguments)}

$(document).ready(function(){
  setInterval(function(){
    $('tr').first().each(function(){
      var epoch = $(this).attr('id').replace(/[^0-9]/g,'');
      $.get('/after/' + epoch,function(html){
        if(html) {
          $('table').prepend(html);
        }
      });
    });
  },1000 * 60 * 5);
});