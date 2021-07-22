var debug = process.env.NODE_ENV !== "production";
const path = require('path');

module.exports = {
    context: __dirname,
    devtool: debug ? "inline-source-map" : null,
    entry: './src/cuauv.jsx',
    output: {
        path: path.resolve(__dirname, 'static'),
        filename: 'bundle.js'
    },
    module: {
        rules: [
            {
                //tell webpack to use jsx-loader for all *.jsx files
                test: /\.jsx$/,
                exclude: /(node_modules)/,
                loader: 'babel-loader',
                options: {
                    presets: ['react', 'es2015'],
                    plugins: ['react-html-attrs', 'transform-class-properties', 'transform-decorators-legacy'],
                }
            },
            {
                //tell webpack to use svg-inline-loader for all *.svg files
                test: /\.svg$/,
                loader: 'svg-inline-loader'
            }
        ]
    },
    resolve: {
        extensions: ['.js', '.jsx']
    },
    target: "web",
    //externals: ["react"],
};
