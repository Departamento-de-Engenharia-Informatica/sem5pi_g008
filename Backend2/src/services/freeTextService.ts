import {Inject, Service} from "typedi";
import IFreeTextService from "./IServices/IFreeTextService";
import config from "../../config";
import IMedicalRecordFreeTextRepo from "./IRepos/IMedicalRecordFreeTextRepo";


@Service()

export default class FreeTextService implements  IFreeTextService{
    
    
    constructor(@Inject(config.repos.medicalRecordFreeText.name) private medicalRecordFreeTextRepo: IMedicalRecordFreeTextRepo) {}
    
    public async addFreeText(medicalRecord: any): Promise<any> {
        
        await this.medicalRecordFreeTextRepo.save(medicalRecord);
        
    }
    
}