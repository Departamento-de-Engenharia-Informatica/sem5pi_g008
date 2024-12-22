import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';
import config from '../../config';

export default async ({expressApp}) => {
  const mongoConnection = await mongooseLoader();
  Logger.info('✌️ DB loaded and connected!');

  const userSchema = {
    // compare with the approach followed in repos and services
    name: 'userSchema',
    schema: '../persistence/schemas/userSchema',
  };

  const roleSchema = {
    // compare with the approach followed in repos and services
    name: 'roleSchema',
    schema: '../persistence/schemas/roleSchema',
  };

  const medicalConditionSchema = {
    // compare with the approach followed in repos and services
    name: 'medicalConditionSchema',
    schema: '../persistence/schemas/medicalConditionSchema',
  };

  const allergySchema = {
    name: 'allergySchema',
    schema: '../persistence/schemas/allergySchema',
  };

  const medicalRecordAllergySchema = {
    name: 'medicalRecordAllergySchema',
    schema: '../persistence/schemas/medicalRecordAllergySchema',
  };

  const medicalRecordConditionSchema = {
    name: 'medicalRecordConditionSchema',
    schema: '../persistence/schemas/medicalRecordConditionSchema',
  };

  const medicalRecordSchema = {
    name: 'medicalRecordSchema',
    schema: '../persistence/schemas/medicalRecordSchema',
  };

  const medicalRecordFreeTextSchema = {
    name: 'medicalRecordFreeTextSchema',
    schema: '../persistence/schemas/medicalRecordFreeTextSchema',
  };

  const roleController = {
    name: config.controllers.role.name,
    path: config.controllers.role.path
  }

  const allergyController = {
    name: config.controllers.allergy.name,
    path: config.controllers.allergy.path
  }

  const medicalRecordController = {
    name: config.controllers.medicalRecord.name,
    path: config.controllers.medicalRecord.path
  }

  const medicalConditionController = {
    name: config.controllers.medicalCondition.name,
    path: config.controllers.medicalCondition.path
  }

  const medicalRecordConditionController = {
    name: config.controllers.medicalRecordCondition.name,
    path: config.controllers.medicalRecordCondition.path
  }

  const roleRepo = {
    name: config.repos.role.name,
    path: config.repos.role.path
  }

  const userRepo = {
    name: config.repos.user.name,
    path: config.repos.user.path
  }

  const medicalConditionRepo = {
    name: config.repos.medicalCondition.name,
    path: config.repos.medicalCondition.path
  }

  const allergyRepo = {
    name: config.repos.allergy.name,
    path: config.repos.allergy.path
  }

  const medicalRecordRepo = {
    name: config.repos.medicalRecord.name,
    path: config.repos.medicalRecord.path
  }

  const medicalRecordAllergyRepo = {
    name: config.repos.medicalRecordAllergy.name,
    path: config.repos.medicalRecordAllergy.path
  }

  const medicalRecordConditionRepo = {
    name: config.repos.medicalRecordCondition.name,
    path: config.repos.medicalRecordCondition.path
  }

  const medicalRecordFreeTextRepo = {
    name: config.repos.medicalRecordFreeText.name,
    path: config.repos.medicalRecordFreeText.path
  }

  const roleService = {
    name: config.services.role.name,
    path: config.services.role.path
  }

  const allergyService = {
    name: config.services.allergy.name,
    path: config.services.allergy.path
  }

  const medicalConditionService = {
    name: config.services.medicalCondition.name,
    path: config.services.medicalCondition.path
  }

  const medicalRecordService = {
    name: config.services.medicalRecord.name,
    path: config.services.medicalRecord.path
  }

  const medicalRecordConditionService = {
    name: config.services.medicalRecordCondition.name,
    path: config.services.medicalRecordCondition.path
  }

  await dependencyInjectorLoader({
    mongoConnection,
    schemas: [
      userSchema,
      roleSchema,
      medicalRecordSchema,
      medicalConditionSchema,
      allergySchema,
      medicalRecordAllergySchema,
      medicalRecordConditionSchema,
      medicalRecordFreeTextSchema
    ],
    controllers: [
      roleController,
      allergyController,
      medicalRecordController,
      medicalConditionController,
      medicalRecordConditionController
    ],
    repos: [
      roleRepo,
      userRepo,
      medicalConditionRepo,
      allergyRepo,
      medicalRecordRepo,
      medicalRecordAllergyRepo,
      medicalRecordConditionRepo,
      medicalRecordFreeTextRepo
    ],
    services: [
      roleService,
      allergyService,
      medicalRecordService,
      medicalConditionService,
      medicalRecordConditionService
    ]
  });
  Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

  await expressLoader({app: expressApp});
  Logger.info('✌️ Express loaded');
};
