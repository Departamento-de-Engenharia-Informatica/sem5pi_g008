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

async function startServer() {
  const app = express();

  await require('./loaders').default({ expressApp: app });

  const medicalProps = {};

  const medicalRecord = MedicalRecord.create(medicalProps);
  const repoMedicalRecord = Container.get(MedicalRecordRepo);
  const savedMedicalRecord = await repoMedicalRecord.save(medicalRecord.getValue());

  const medicalConditionProps = {
    condition: "Covid3",
  };

  const medicalCondition = MedicalCondition.create(medicalConditionProps);
  const repoMedicalCondition = Container.get(MedicalConditionRepo);
  const savedMedicalCondition = await repoMedicalCondition.save(medicalCondition.getValue());

  const medicalRecordConditionProps = {
    condition: savedMedicalCondition,
    medicalRecord: savedMedicalRecord,
    doctorId: "1",
    comment: "Covid",
  };

  const medicalRecordCondition = MedicalRecordCondition.create(medicalRecordConditionProps);
  const repoMedicalRecordCondition = Container.get(MedicalRecordConditionRepo);
  await repoMedicalRecordCondition.save(medicalRecordCondition.getValue());

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
