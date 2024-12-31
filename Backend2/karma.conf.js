const path = require('path');

module.exports = function(config) {
  config.set({

    // Base path to resolve all patterns (e.g., files, exclude)
    basePath: '',

    // Frameworks to use
    frameworks: ['jasmine','sinon'],

    // List of files/patterns to load in the browser
    files: [
      { pattern: 'tests/**/*.spec.ts', type: 'module' } // Explicitly define as TypeScript
    ],

    // List of files/patterns to exclude
    exclude: [],

    // Preprocess matching files before serving them to the browser
    preprocessors: {
      'tests/**/*.spec.ts': ['webpack'], // Use Webpack to process TypeScript files
    },

    // Webpack configuration
    webpack: {
      mode: 'development',
      module: {
        rules: [
          {
            test: /\.ts$/,
            use: 'ts-loader',
            exclude: /node_modules/,
          },
        ],
      },
      resolve: {
        extensions: ['.ts', '.js'],
        alias: {
          config: path.resolve(__dirname, 'src/config.ts'), // Add alias for config
        },
        fallback: {
          os: require.resolve('os-browserify/browser'),
          crypto: require.resolve('crypto-browserify'),
          stream: require.resolve('stream-browserify'),
          vm: require.resolve('vm-browserify'),
          util: require.resolve('util/'),
        },
      },
    },

    reporters: ['progress', 'kjhtml'],
    client: {
      clearContext: false,
    },

    port: 9876,

    colors: true,

    autoWatch: true,

    // Start these browsers
    browsers: [],

    // Continuous Integration mode
    singleRun: false,

    // Concurrency level
    concurrency: Infinity,
  });
};
