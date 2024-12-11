import 'reflect-metadata'; // We need this in order to use @Decorators

import config from '../config';

import express from 'express';

import Logger from './loaders/logger';
import {UserEmail} from "./domain/userEmail";
import {UserPassword} from "./domain/userPassword";
import {Role} from "./domain/role";
import IRoleDTO from "./dto/IRoleDTO";
import RoleRepo from "./repos/roleRepo";
import {Container} from "typedi";
import UserRepo from "./repos/userRepo";
import {User} from "./domain/user";
import {MedicalCondition} from "./domain/MedicalCondition/MedicalCondition";
import MedicalConditionRepo from "./repos/MedicalConditionRepo";
import {UniqueEntityID} from "./core/domain/UniqueEntityID";

async function startServer() {
  const app = express();

  await require('./loaders').default({ expressApp: app });

  app.listen(config.port, () => {

    console.log("Server listening on port: " + config.port);
    
    
    const medCondProps = {
      condition: "Condição"
    };
    
    const medCond = MedicalCondition.create(medCondProps);
    
    const medCondRepo = Container.get(MedicalConditionRepo);
    
    medCondRepo.save(medCond.getValue());
    
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