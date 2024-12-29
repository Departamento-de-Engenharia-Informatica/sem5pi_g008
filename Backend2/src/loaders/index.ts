import expressLoader from './express';
import dependencyInjectorLoader from './dependencyInjector';
import mongooseLoader from './mongoose';
import Logger from './logger';
import config from '../../config';

export default async ({expressApp}) => {
    const mongoConnection = await mongooseLoader();
    Logger.info('✌️ DB loaded and connected!');

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
    const medicalRecordFamilyHistorySchema = {
        name: 'medicalRecordFamilyHistorySchema',
        schema: '../persistence/schemas/medicalRecordFamilyHistorySchema',
    };



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
    const medicalRecordFamilyHistoryRepo = {
        name: config.repos.medicalRecordFamilyHistory.name,
        path: config.repos.medicalRecordFamilyHistory.path
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

    await dependencyInjectorLoader({
        mongoConnection,
        schemas: [
            medicalRecordSchema,
            medicalConditionSchema,
            allergySchema,
            medicalRecordAllergySchema,
            medicalRecordConditionSchema,
            medicalRecordFreeTextSchema,
            medicalRecordFamilyHistorySchema
        ],
        controllers: [
            allergyController,
            medicalRecordController,
            medicalConditionController
        ], 
        repos: [
            medicalConditionRepo,
            allergyRepo,
            medicalRecordRepo,
            medicalRecordAllergyRepo,
            medicalRecordConditionRepo,
            medicalRecordFreeTextRepo,
            medicalRecordFamilyHistoryRepo
        ],
        services: [
            allergyService,
            medicalRecordService,
            medicalConditionService,
            medicalConditionService
        ]
    });
    Logger.info('✌️ Schemas, Controllers, Repositories, Services, etc. loaded');

    await expressLoader({app: expressApp});
    Logger.info('✌️ Express loaded');
};
