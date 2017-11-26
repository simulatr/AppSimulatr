//hideChild = function(){
//  $('#parameters-details .grid-content').click(function(){
//    $(this).find('.inner-desc-details').toggleClass('shinyjs-hide');
//  });
//}




shinyjs.hideChild = function(parent, child){
  $(parent).click(function(){
    $(this).find(child).toggleClass('shinyjs-hide');
  });
}