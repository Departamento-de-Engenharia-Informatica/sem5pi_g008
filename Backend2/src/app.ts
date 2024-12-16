import 'reflect-metadata';

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';
import {Container} from "typedi";
import MedicalConditionRepo from "./repos/MedicalConditionRepo";
import {MedicalCondition} from "./domain/MedicalCondition/MedicalCondition";
import {Allergy} from "./domain/Allergy/Allergy";
import AllergyRepo from "./repos/AllergyRepo";
import {MedicalRecord} from "./domain/MedicalRecord/MedicalRecord";
import {MedicalRecordAllergy} from "./domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {MedicalRecordCondition} from "./domain/MedicalRecordCondition/MedicalRecordCondition";
import {MedicalRecordFreeText} from "./domain/MedicalRecordFreeText/MedicalRecordFreeText";
import MedicalRecordRepo from "./repos/MedicalRecordRepo";


async function startServer() {
  const app = express();

  await require('./loaders').default({ expressApp: app });

  const medCondProps = {
    condition: "Conditionnnnee"
  };

  const medCod = MedicalCondition.create(medCondProps);

  const med = Container.get(MedicalConditionRepo);

  const allergyProps = {
    allergy: "allll"
  };

  const allCod = Allergy.create(allergyProps);

  const all = Container.get(AllergyRepo);


  const medRecAllergyProps = {
    allergy: allCod.getValue(),
    doctorId: "123",
    comment: "comment"
  };

  const medRecAllergy = MedicalRecordAllergy.create(medRecAllergyProps);

  const medRecConditionsProps = {
    condition: medCod.getValue(),
    doctorId: "123",
    comment: "comment"
  }

  const medRecCondition = MedicalRecordCondition.create(medRecConditionsProps);

  const freeTextProps = {
    doctorId: "123",
    comment: "comment"
  }

  const freeText = MedicalRecordFreeText.create(freeTextProps);

  const medicalProps = {

    medicalRecordConditions: [medRecCondition],
    medicalRecordAllergies: [medRecAllergy],
    freeText: [freeText]
  };

  const medicalRecord = MedicalRecord.create(medicalProps);

  const repoMedicalRecord = Container.get(MedicalRecordRepo);

  repoMedicalRecord.save(medicalRecord.getValue());

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
