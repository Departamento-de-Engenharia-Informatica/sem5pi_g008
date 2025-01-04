import 'reflect-metadata';

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';
import {Container} from "typedi";
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
import {MedicalRecord} from "./domain/MedicalRecord/MedicalRecord";
import {Designation} from "./domain/Shared/designation";
import {Code} from "./domain/Shared/code";
import {Description} from "./domain/Shared/description";


async function seedData() {

  const medicalRecordProps = {}
  const medicalRecordDomain = MedicalRecord.create(medicalRecordProps).getValue();
  const medicalRecordRepo = Container.get(MedicalRecordRepo);
  await medicalRecordRepo.save(medicalRecordDomain, "20250100007");

  const medicalRecordDomain2 = MedicalRecord.create(medicalRecordProps).getValue();
  await medicalRecordRepo.save(medicalRecordDomain2, "20250100003");

  const medicalRecordDomain3 = MedicalRecord.create(medicalRecordProps).getValue();
  await medicalRecordRepo.save(medicalRecordDomain3, "20250100004");

  const medicalRecordDomain4 = MedicalRecord.create(medicalRecordProps).getValue();
  await medicalRecordRepo.save(medicalRecordDomain4, "20250100005");


  const medicalRecord = await medicalRecordRepo.getMedicalRecordByDomainId("20250100005");

  const medicalRecordId = medicalRecord.props._id;

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

  const medicalConditionRepo = Container.get(MedicalConditionRepo);

  const medicalCondition = MedicalCondition.create(medicalConditionProps);
  const medicalCondition2 = MedicalCondition.create(medicalConditionProps2);
  const medC = await medicalConditionRepo.save(medicalCondition.getValue());
  const medC2 = await medicalConditionRepo.save(medicalCondition2.getValue());

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
    code: Code.create("AL10").getValue(),
    designation: Designation.create("PENICILLIN").getValue(),
    description: Description.create("PENICILLIN IS A BAD ALLERGY").getValue(),
    effects: ["RASH", "ITCHING"]
  };

  const allergyProps2 = {
    code: Code.create("AL11").getValue(),
    designation: Designation.create("ASPIRIN").getValue(),
    description: Description.create("ASPIRIN IS A BAD ALLERGY").getValue(),
    effects: ["HEADACHE", "NAUSEA"]
  };

  const allergy = Allergy.create(allergyProps);
  const allergy2 = Allergy.create(allergyProps2);

  const a1 = await medicalAllergyRepo.save(allergy.getValue());
  const a2 = await medicalAllergyRepo.save(allergy2.getValue());

  const medicalRecordAllergyRepo = Container.get(MedicalRecordAllergyRepo);
  const medicalRecordAllergyProps = {
    allergyId: a1.props._id,
    medicalRecordId: medicalRecordId,
    doctorId: "N202400005",
    comment: "THE GUY IS ALLERGIC TO PENICILLIN"
  }

  const medicalRecordAllergyProps2 = {
    allergyId: a2.props._id,
    medicalRecordId: medicalRecordId,
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

  await require('./loaders').default({expressApp: app});

  try {
      await seedData();
  } catch (error) {
    console.log("Error seeding data: ", error);
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
