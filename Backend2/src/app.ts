import 'reflect-metadata';

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';
import { Container } from "typedi";
import MedicalRecordRepo from "./repos/MedicalRecordRepo";
import {MedicalRecordFreeText} from "./domain/MedicalRecordFreeText/MedicalRecordFreeText";
import MedicalRecordFreeTextRepo from "./repos/MedicalRecordFreeTextRepo";
import MedicalRecordConditionRepo from "./repos/MedicalRecordConditionRepo";
import {MedicalRecordCondition} from "./domain/MedicalRecordCondition/MedicalRecordCondition";
import {MedicalCondition} from "./domain/MedicalCondition/MedicalCondition";
import MedicalConditionRepo from "./repos/MedicalConditionRepo";
import AllergyRepo from "./repos/AllergyRepo";
import {Allergy} from "./domain/Allergy/Allergy";
import MedicalRecordAllergyRepo from "./repos/MedicalRecordAllergyRepo";
import {MedicalRecordAllergy} from "./domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {Code} from "./domain/MedicalCondition/code";
import {Designation} from "./domain/MedicalCondition/designation";
import {Description} from "./domain/MedicalCondition/description";
import {MedicalRecord} from "./domain/MedicalRecord/MedicalRecord";


async function seedData(medicalRecordId: string) {
  const medicalRecordFreeTextRepo = Container.get(MedicalRecordFreeTextRepo);
  const medicalRecordFreeTextProps = {
    medicalRecord: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY IS NEARLY DEAD"
  }

  const medicalRecordFreeTextProps2 = {
    medicalRecord: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY IS A LITTLE BIT BETTER"
  }

  const medicalRecordFreeText = MedicalRecordFreeText.create(medicalRecordFreeTextProps);
  const medicalRecordFreeText2 = MedicalRecordFreeText.create(medicalRecordFreeTextProps2);
  await medicalRecordFreeTextRepo.save(medicalRecordFreeText.getValue());
  await medicalRecordFreeTextRepo.save(medicalRecordFreeText2.getValue());


  const medicalRecordRepo = Container.get(MedicalConditionRepo);

  const medicalConditionProps = {
    code: Code.create("CA12").getValue(),
    designation: Designation.create("CANCER").getValue(),
    description: Description.create("CANCER IS A BAD CONDITION").getValue(),
    symptomsList: ["COUGH", "HEADACHE"]
    
  };

  const medicalConditionProps2 = {
    code: Code.create("HA12").getValue(),
    designation: Designation.create("HEART ATTACK").getValue(),
    description: Description.create("HEART ATTACK IS A BAD CONDITION").getValue(),
    symptomsList: ["CHEST PAIN", "SHORTNESS OF BREATH"]
  };

  const medicalCondition = MedicalCondition.create(medicalConditionProps);
  const medicalCondition2 = MedicalCondition.create(medicalConditionProps2);
  const medC = await medicalRecordRepo.save(medicalCondition.getValue());
  const medC2 = await medicalRecordRepo.save(medicalCondition2.getValue());

  const medicalRecordConditionRepo = Container.get(MedicalRecordConditionRepo);
  const medicalRecordConditionProps = {
    condition: medC.props._id,
    medicalRecord: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY HAS CANCER"
  }

  const medicalRecordConditionProps2 = {
    condition: medC2.props._id,
    medicalRecord: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY HAD AN HEART ATTACK"
  }

  const medicalRecordCondition = MedicalRecordCondition.create(medicalRecordConditionProps);
  const medicalRecordCondition2 = MedicalRecordCondition.create(medicalRecordConditionProps2);
  await medicalRecordConditionRepo.save(medicalRecordCondition.getValue());
  await medicalRecordConditionRepo.save(medicalRecordCondition2.getValue());

  const medicalAllergyRepo = Container.get(AllergyRepo);
  const allergyProps = {
    allergy: "PENICILLIN"
  };

  const allergyProps2 = {
    allergy: "ASPIRIN"
  };

  const allergy = Allergy.create(allergyProps);
  const allergy2 = Allergy.create(allergyProps2);

  const a1 = await medicalAllergyRepo.save(allergy.getValue());
  const a2 = await medicalAllergyRepo.save(allergy2.getValue());

  const medicalRecordAllergyRepo = Container.get(MedicalRecordAllergyRepo);
  const medicalRecordAllergyProps = {
    allergy: a1.props._id,
    medicalRecord: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY IS ALLERGIC TO PENICILLIN"
  }

  const medicalRecordAllergyProps2 = {
    allergy: a2.props._id,
    medicalRecord: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY IS ALLERGIC TO ASPIRIN"
  }

  const medicalRecordAllergy = MedicalRecordAllergy.create(medicalRecordAllergyProps);
  const medicalRecordAllergy2 = MedicalRecordAllergy.create(medicalRecordAllergyProps2);

  await medicalRecordAllergyRepo.save(medicalRecordAllergy.getValue());
  await medicalRecordAllergyRepo.save(medicalRecordAllergy2.getValue());
}

async function startServer() {
  const app = express();

  await require('./loaders').default({ expressApp: app });

  const medicalRecordProps = {
  }
  
    const medicalRecordd = MedicalRecord.create(medicalRecordProps).getValue();
  
  const medicalRecordRepo = Container.get(MedicalRecordRepo);
  
    const medicalRecordSaved = await medicalRecordRepo.save(medicalRecordd, "20241200007");
  
  const medicalRecord =  await medicalRecordRepo.getMedicalRecordById("20241200007");
  
  if(medicalRecord !== null) {
    await seedData(medicalRecord.props._id);
  }



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
