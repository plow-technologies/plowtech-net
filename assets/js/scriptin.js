// stellar
$.stellar({
	horizontalScrolling: false
});

// jquery
var $window = $(window);

$window.scroll(function (event) {
	var scrollTop = $window.scrollTop();

	$('#header').css('opacity', 1 - scrollTop*.0030);
	
});


