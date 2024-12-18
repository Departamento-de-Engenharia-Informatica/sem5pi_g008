import {Inject, Service} from "typedi";
import IMedicalRecordService from "./IServices/IMedicalRecordService";
import config from "../../config";
import IMedicalRecordRepo from "./IRepos/IMedicalRecordRepo";
import IMedicalRecordDTO from "../dto/IMedicalRecordDTO";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";


@Service()
export default class MedicalRecordService implements IMedicalRecordService{
    constructor(
        @Inject(config.repos.medicalRecord.name) private medicalRecordRepo:IMedicalRecordRepo
    ) {}

    public async createMedicalRecord(medicalRecordId:string): Promise<void> {
      const medicalProps = {};
      const medicalRecord = MedicalRecord.create(medicalProps);
      await this.medicalRecordRepo.save(medicalRecord.getValue(), medicalRecordId);
    }

}

