import {Inject, Service} from "typedi";
import IMedicalRecordRepo from "../services/IRepos/IMedicalRecordRepo";
import {Document, Model} from "mongoose";
import {IMedicalRecordPersistence} from "../dataschema/IMedicalRecordPersistence";
import {MedicalRecord} from "../domain/MedicalRecord/MedicalRecord";
import {MedicalRecordMapper} from "../mappers/MedicalRecordMapper";


@Service()
export default class MedicalRecordRepo implements IMedicalRecordRepo{
    constructor(@Inject('medicalRecordSchema') private medicalRecordSchema: Model<IMedicalRecordPersistence & Document>,) {
    }

    exists(t: MedicalRecord): Promise<boolean> {
        return Promise.resolve(false);
    }

    save(medicalRecord: MedicalRecord): Promise<MedicalRecord> {

      const id = this.getLastId();


      const rawMedicalRecord: any = MedicalRecordMapper.toPersistence(medicalRecord, id);

      const medicalRecordCreated = new this.medicalRecordSchema(rawMedicalRecord);

        return Promise.resolve(undefined);
    }


  public async getLastId(): Promise<number> {
    const lastElement = await this.medicalRecordSchema.find().sort({domainId: -1}).limit(1);

    if (lastElement.length === 0) {
      return 0;
    }

    return 1;
  }

}
