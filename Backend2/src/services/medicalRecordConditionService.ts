import {Inject, Service} from "typedi";
import config from "../../config";
import IMedicalRecordConditionRepo from "./IRepos/IMedicalRecordConditionRepo";
import IMedicalRecordConditionService from "./IServices/IMedicalRecordConditionService";

@Service()
export default class MedicalRecordConditionService implements IMedicalRecordConditionService{
    constructor(
        @Inject(config.repos.medicalRecordCondition.name) private medicalRecordMedicalConditionRepo: IMedicalRecordConditionRepo
    ) {
    }
    
    public async getMedicalRecordConditions(medicalRecordId: string): Promise<any> {
        
        const list = await this.medicalRecordMedicalConditionRepo.getMedicalRecordConditions(medicalRecordId);
    }
}