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

  const medicalProps = {};

  const medicalRecord = MedicalRecord.create(medicalProps);
  const repoMedicalRecord = Container.get(MedicalRecordRepo);
  const savedMedicalRecord = await repoMedicalRecord.save(medicalRecord.getValue());

  const allergyProps = {
    allergy: "Peanuts6",
  };

  const allergy = Allergy.create(allergyProps);
  const allergyRepo = Container.get(AllergyRepo);
  const savedAllergy = await allergyRepo.save(allergy.getValue());

  const medicalRecordAllergyProps = {
    allergy: savedAllergy.props._id.toString(),
    medicalRecord: savedMedicalRecord.props._id.toString(),
    doctorId: "1",
    comment: "Allergic",
  };

  const medicalRecordAllergyResult = MedicalRecordAllergy.create(medicalRecordAllergyProps);
  const repoMedicalRecordAllergy = Container.get(MedicalRecordAllergyRepo);
  await repoMedicalRecordAllergy.save(medicalRecordAllergyResult.getValue());


  const medicalFreeTextProps = {
    medicalRecord: savedMedicalRecord.props._id.toString(),
    doctorId: "2",
    comment: "Free text",
  };

  const freeTextResult = MedicalRecordFreeText.create(medicalFreeTextProps);
  const repoMedicalFreeText = Container.get(MedicalRecordFreeTextRepo);
  await repoMedicalFreeText.save(freeTextResult.getValue());

  const medicalConditionProps = {
    condition: "Covid22",
  };

  const medicalCondition = MedicalCondition.create(medicalConditionProps);
  const medicalConditionRepo = Container.get(MedicalConditionRepo);
  const savedMedicalCondition = await medicalConditionRepo.save(medicalCondition.getValue());

  const medicalRecordConditionProps = {
    condition: savedMedicalCondition.props._id.toString(),
    medicalRecord: savedMedicalRecord.props._id.toString(),
    doctorId: "1",
    comment: "Condition",
  };

  const medicalRecordConditionResult = MedicalRecordCondition.create(medicalRecordConditionProps);
  const repoMedicalRecordCondition = Container.get(MedicalRecordConditionRepo);
  await repoMedicalRecordCondition.save(medicalRecordConditionResult.getValue());

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
