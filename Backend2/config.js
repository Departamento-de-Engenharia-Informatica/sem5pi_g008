import dotenv from 'dotenv';

// Set the NODE_ENV to 'development' by default
process.env.NODE_ENV = process.env.NODE_ENV || 'development';

const envFound = dotenv.config();
if (!envFound) {
  // This error should crash whole process

  throw new Error("⚠️  Couldn't find .env file  ⚠️");
}

export default {
  /**
   * Your favorite port : optional change to 4000 by JRT
   */
  port: parseInt(process.env.PORT, 10) || 4000,

  /**
   * That long string from mlab
   */
  databaseURL: process.env.MONGODB_URI || "mongodb://mongoadmin:0660b65e64513ce6c40897aa@vsgate-s1.dei.isep.ipp.pt:10782/admin",

  /**
   * Your secret sauce
   */
  jwtSecret: process.env.JWT_SECRET || "my sakdfho2390asjod$%jl)!sdjas0i secret",

  /**
   * Used by winston logger
   */
  logs: {
    level: process.env.LOG_LEVEL || 'info',
  },

  /**
   * API configs
   */
  api: {
    prefix: '/api',
  },

  controllers: {
    role: {
      name: "RoleController",
      path: "../controllers/roleController"
    },
    allergy: {
      name: "AllergyController",
      path: "../controllers/AllergyController"
    },
    medicalCondition: {
      name: "MedicalConditionController",
      path: "../controllers/MedicalConditionController"
    },
    medicalRecord: {
      name:"MedicalRecordController",
      path:"../controllers/MedicalRecordController"
    },
      medicalRecordCondition: {
          name: "MedicalRecordConditionController",
          path: "../controllers/MedicalRecordConditionController"
      }
  },

  repos: {
    role: {
      name: "RoleRepo",
      path: "../repos/roleRepo"
    },
    user: {
      name: "UserRepo",
      path: "../repos/userRepo"
    },
    medicalCondition: {
        name: "MedicalConditionRepo",
        path: "../repos/MedicalConditionRepo"
    },
    allergy: {
      name: "AllergyRepo",
      path: "../repos/AllergyRepo"
    },
    medicalRecord: {
      name:"MedicalRecordRepo",
      path:"../repos/MedicalRecordRepo"
    },
    medicalRecordAllergy: {
      name: "MedicalRecordAllergyRepo",
      path: "../repos/MedicalRecordAllergyRepo"
    },
    medicalRecordCondition: {
      name: "MedicalRecordConditionRepo",
      path: "../repos/MedicalRecordConditionRepo"
    },
    medicalRecordFreeText: {
      name: "MedicalRecordFreeTextRepo",
      path: "../repos/MedicalRecordFreeTextRepo"
    },
  },

  services: {
    role: {
      name: "RoleService",
      path: "../services/roleService"
    },
    allergy: {
      name: "AllergyService",
      path: "../services/AllergyService"
    },
    medicalCondition: {
      name: "MedicalConditionService",
      path: "../services/MedicalConditionService"
    },
    medicalRecord:{
      name: "MedicalRecordService",
      path:"../services/MedicalRecordService"
    },
    medicalRecordCondition: {
        name: "MedicalRecordConditionService",
        path: "../services/MedicalRecordConditionService"
    }
  },

  Google: {
    ClientId: "367986401250-0qiqdf0cot2g1bpu9l0qppcf33633amd.apps.googleusercontent.com",
    Client_Secret: "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"
  },

  Backend1: {
    URL: "http://localhost:5001"
  }
};
