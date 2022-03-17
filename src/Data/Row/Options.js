exports.unsafeOptionsGet = function (nothing) {
	return function (just) {
		return function (key) {
			return function (r) {
				return Object.hasOwnProperty.call(r, key) ? just(r[key]) : nothing;
			};
		};
	};
};
exports.bcopyRowOptions = function (rec) {
	var copy = {};
	for (var key in rec) {
		if ({}.hasOwnProperty.call(rec, key)) {
			copy[key] = rec[key];
		}
	}
	return copy;
};

exports.bunsafeInsert = function (l) {
	return function (a) {
		return function (rec) {
			rec[l] = a;
			return rec;
		};
	};
};

exports.bunsafeModify = function (l) {
	return function (f) {
		return function (rec) {
			if (Object.hasOwnProperty.call(rec, l)) {
				rec[l] = f(rec[l]);
			}
			return rec;
		};
	};
};

exports.bunsafeDelete = function (l) {
	return function (rec) {
		delete rec[l];
		return rec;
	};
};

exports.bunsafeRename = function (l1) {
	return function (l2) {
		return function (rec) {
			if (Object.hasOwnProperty.call(rec, l1)) {
				rec[l2] = rec[l1];
				delete rec[l1];
			}
			return rec;
		};
	};
};

exports.unsafeMegamap = function (r1) {
	return function (r2) {
		var copy = {};
		for (var key in r1) {
			if (Object.hasOwnProperty.call(r1, key)) {
				copy[key] = r2[key](r1[key]);
			}
		}
		return copy;
	};
};
