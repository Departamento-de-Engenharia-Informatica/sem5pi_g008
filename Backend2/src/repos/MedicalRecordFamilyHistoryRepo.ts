import {Inject, Service} from "typedi";
import {Document, Model} from "mongoose";
import {MedicalRecordFamilyHistory} from "../domain/MedicalRecordFamilyHistory/MedicalRecordFamilyHistory";
import {MedicalRecordFamilyHistoryMap} from "../mappers/MedicalRecordFamilyHistoryMapper";
import {IMedicalRecordFamilyHistoryPersistence} from "../dataschema/IMedicalRecordFamilyHistoryPersistence";
import IMedicalRecordFamilyHistoryRepo from "../services/IRepos/IMedicalRecordFamilyHistoryRepo";

@Service()
export default class MedicalRecordFamilyHistoryRepo implements IMedicalRecordFamilyHistoryRepo{
  constructor(@Inject('medicalRecordFamilyHistorySchema') private medicalRecordFamilyHistorySchema: Model<IMedicalRecordFamilyHistoryPersistence & Document>,) {
  }

  exists(t: MedicalRecordFamilyHistory): Promise<boolean> {
    //TODO:Implement exists method
    return Promise.resolve(false);
  }

  public async save(medicalRecordFamilyHistory: MedicalRecordFamilyHistory): Promise<MedicalRecordFamilyHistory> {

    const id = await this.getLastId();

    const rawMedicalRecordFamilyHistory: any = MedicalRecordFamilyHistoryMap.toPersistence(medicalRecordFamilyHistory, id);

    const medicalRecordFamilyHistoryCreated = await this.medicalRecordFamilyHistorySchema.create(rawMedicalRecordFamilyHistory);

    return MedicalRecordFamilyHistoryMap.toDomain(medicalRecordFamilyHistoryCreated);
  }


  public async getLastId(): Promise<number> {

    let number = 1;

    const medicalRecord = await this.medicalRecordFamilyHistorySchema.find();

    if (medicalRecord.length > 0) {
      number = medicalRecord.length + 1;
    }

    return number;
  }
}
