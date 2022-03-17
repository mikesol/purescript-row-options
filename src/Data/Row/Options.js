exports.unsafeOptionsGet = function (nothing) {
	return function (just) {
		return function (key) {
			return function (r) {
				return Object.hasOwnProperty.call(r, key) ? just(r[key]) : nothing;
			};
		};
	};
};
