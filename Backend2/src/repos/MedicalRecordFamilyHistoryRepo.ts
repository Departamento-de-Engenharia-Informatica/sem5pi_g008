import {Inject, Service} from "typedi";
import mongoose, {Document, Model} from "mongoose";
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
  public async saveFamilyHistory(id: string, familylist: any): any {
    console.log('FAMILY LIST', familylist);
    const rawFamilyHistory: any = MedicalRecordFamilyHistoryMap.toPersistence(id, familylist);
    console.log('RAW FAMILY HISTORY', rawFamilyHistory);
    const familyHistoryCreated = this.medicalRecordFamilyHistorySchema.create(rawFamilyHistory);
    console.log('FAMILY HISTORY CREATED', familyHistoryCreated);
    return MedicalRecordFamilyHistoryMap.toDomain(familyHistoryCreated);
  }
  public async save(medicalRecordFamilyHistory: MedicalRecordFamilyHistory): Promise<MedicalRecordFamilyHistory> {
    const id = await this.getLastId();

    const rawMedicalRecordFamilyHistory: any = MedicalRecordFamilyHistoryMap.toPersistence(medicalRecordFamilyHistory, id);
    

    const medicalRecordFamilyHistoryCreated = await this.medicalRecordFamilyHistorySchema.create(rawMedicalRecordFamilyHistory);

    return MedicalRecordFamilyHistoryMap.toDomain(medicalRecordFamilyHistoryCreated);
  }
  public async getMedicalRecordFamilyHistoryWithIds(medicalRecordId: string): Promise<any[]> {

    const medicalRecordFamilyHistory = await this.medicalRecordFamilyHistorySchema.find({ medicalRecordId: medicalRecordId });

    medicalRecordFamilyHistory.map(MedicalRecordFamilyHistoryMap.toDomain);

    return medicalRecordFamilyHistory;
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
