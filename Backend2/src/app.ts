import 'reflect-metadata';

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';
import { Container } from "typedi";
import MedicalRecordRepo from "./repos/MedicalRecordRepo";
import { MedicalRecord } from "./domain/MedicalRecord/MedicalRecord";
import {MedicalCondition} from "./domain/MedicalCondition/MedicalCondition";
import MedicalConditionRepo from "./repos/MedicalConditionRepo";
import {MedicalRecordCondition} from "./domain/MedicalRecordCondition/MedicalRecordCondition";
import MedicalRecordConditionRepo from "./repos/MedicalRecordConditionRepo";
import {Allergy} from "./domain/Allergy/Allergy";
import AllergyRepo from "./repos/AllergyRepo";
import {MedicalRecordAllergy} from "./domain/MedicalRecordAllergy/MedicalRecordAllergy";
import MedicalRecordAllergyRepo from "./repos/MedicalRecordAllergyRepo";
import {MedicalRecordFreeText} from "./domain/MedicalRecordFreeText/MedicalRecordFreeText";
import MedicalRecordFreeTextRepo from "./repos/MedicalRecordFreeTextRepo";

async function startServer() {
  const app = express();

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
    });
}

startServer();
