/**
 * Created by Denny on 18.05.2015.
 */

/**
 * sort array of objects
 * @param field attribute to sort
 * @param reverse reverse output
 * @param [primer] function aplied to attribute
 * @returns {Function}
 */
function sort_by(field, reverse, primer) {
    var key = function (x) {
        return primer ? primer(x[field]) : x[field]
    };
    return function (a, b) {

        var A = key(a), B = key(b);
        if (A < B) {
            return -1 * [-1, 1][+!!reverse]
        }
        else {
            if (A > B) {
                return +1 * [-1, 1][+!!reverse]
            }
            else {
                return 0
            }
        }
    }
}