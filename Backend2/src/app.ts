import 'reflect-metadata'; // We need this in order to use @Decorators

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';
import cookieParser from 'cookie-parser'; // Import cookie-parser


async function startServer() {
  const app = express();

  app.use(cookieParser());

  await require('./loaders').default({ expressApp: app });

  app.listen(config.port, () => {

    console.log("Server listening on port: " + config.port);

    Logger.info(`
      ################################################
          Server in url ${config.api}
      🛡️  Server listening on port: ${config.port} 🛡️
      ################################################
    `);
    })
    .on('error', (err) => {
      Logger.error(err);
      process.exit(1);
      return;
  });
}

startServer();
