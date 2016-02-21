$(document).ready(function(){

    $(".start-slide").click(function(){
        $("#myCarousel").carousel('cycle');
    });

    // logo blur
	var $window = $(window);

	$window.scroll(function (event) {
		var scrollTop = $window.scrollTop();

		$('#header').css('opacity', 1 - scrollTop*.0030);
	});

});