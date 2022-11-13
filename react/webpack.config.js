const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
module.exports = {
    mode: 'development',
    entry: './src/index.tsx',
    experiments: {
        asyncWebAssembly: true,
        syncWebAssembly: true
    },
    resolve: {
        extensions: ['.js', '.ts', '.tsx', '.css']
    },
    module: {
        rules: [
            {
                test: /\.(png|jpg|gif)$/i,
                generator: {
                    filename: 'images/[name][ext][query]'
                },
                type: 'asset/resource'
            },            
            {
                test: /\.(ts|tsx|js)$/,
                exclude: /node_modules/,
                use: [
                    {
                        loader: 'babel-loader',
                        options: { presets: ['@babel/preset-env', '@babel/preset-react', '@babel/preset-typescript'] },
                    },
                ],
            },
        ]
    },
    output: {
        path: `${__dirname}/dist`,
        filename: "main.js"
    },
    plugins: [
        new HtmlWebpackPlugin({ template: './src/index.html' }),
    ]
}
