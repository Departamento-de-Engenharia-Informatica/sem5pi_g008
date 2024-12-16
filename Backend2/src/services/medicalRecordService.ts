import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";


@Service()

export default class MedicalRecordService implements IMedicalRecordService{
    
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo
    ) {}
    
    
    
}
    
