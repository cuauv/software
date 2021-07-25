var debug = process.env.NODE_ENV !== "production";
const path = require('path');

module.exports = {
    context: __dirname,
    devtool: debug ? "inline-source-map" : null,
    entry: './src/index.js',
    output: {
        path: path.resolve(__dirname, 'static/js/'),
        filename: 'bundle.js'
    },
    module: {
        rules: [
        ]
    },
    resolve: {
        extensions: ['.js']
    },
    target: "web",
};
