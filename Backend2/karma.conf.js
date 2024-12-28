module.exports = function(config) {
  config.set({

    // Base path to resolve all patterns (e.g., files, exclude)
    basePath: '',

    // Frameworks to use
    frameworks: ['jasmine'],

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
        extensions: ['.ts', '.js'], // Resolve TypeScript and JavaScript files
      },
    },

    // Test results reporter to use
    reporters: ['progress', 'kjhtml'], // Use Jasmine HTML reporter for better UI
    client: {
      clearContext: false, // Keep Jasmine Spec Runner output visible in the browser
    },

    // Web server port
    port: 9876,

    // Enable/disable colors in the output (reporters and logs)
    colors: true,

    // Level of logging
    logLevel: config.LOG_INFO,

    // Enable/disable watching files and executing tests on file changes
    autoWatch: true,

    // Start these browsers
    browsers: ['Chrome'],

    // Continuous Integration mode
    singleRun: false,

    // Concurrency level
    concurrency: Infinity,
  });
};
