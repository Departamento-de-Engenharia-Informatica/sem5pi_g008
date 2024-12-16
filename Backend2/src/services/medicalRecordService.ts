import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import IMedicalRecordDTO from "../dto/IMedicalRecordDTO";


@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo
    ) {}

    createMedicalRecord(medicalRecord: IMedicalRecordDTO): Promise<any> {
        return Promise.resolve(undefined);
    }

    updateMedicalRecord(medicalRecord: IMedicalRecordDTO): Promise<any> {
        return Promise.resolve(undefined);
    }
    
   
    
    
    
}
    
